const express = require('express');
const path = require('path');
const fs = require('fs-extra');
const CobolParser = require('../utils/cobolParser');

const router = express.Router();

/**
 * POST /api/analyze/summary
 * Analyze uploaded COBOL files and return summary
 */
router.post('/summary', async (req, res) => {
  try {
    const { jobId } = req.body;
    
    if (!jobId) {
      return res.status(400).json({ error: 'Job ID is required' });
    }

    const jobDir = path.join(__dirname, '../uploads', jobId);
    
    if (!fs.existsSync(jobDir)) {
      return res.status(404).json({ error: 'Job not found' });
    }

    const parser = new CobolParser();
    const analysis = parser.analyzeDirectory(jobDir);

    // Save analysis results for later retrieval
    const analysisPath = path.join(jobDir, 'analysis.json');
    await fs.writeJson(analysisPath, analysis);

    // Format response to match frontend expectations
    const response = {
      jobId,
      timestamp: new Date().toISOString(),
      fileClassification: [
        { 
          category: 'COBOL Programs', 
          count: analysis.summary.programs.length, 
          files: analysis.summary.programs 
        },
        { 
          category: 'Copybooks', 
          count: analysis.summary.copybooks.length, 
          files: analysis.summary.copybooks 
        },
        { 
          category: 'Total LOC', 
          count: analysis.summary.totalLOC, 
          files: [] 
        },
        { 
          category: 'External CALLs', 
          count: analysis.summary.dependencies.call.length, 
          files: analysis.summary.dependencies.call.map(call => `CALL "${call}"`) 
        },
        { 
          category: 'COPY statements', 
          count: analysis.summary.dependencies.copy.length, 
          files: analysis.summary.dependencies.copy.map(copy => `COPY "${copy}"`) 
        }
      ],
      extractedInsights: [
        { 
          type: 'PERFORM Patterns', 
          value: `THRU: ${analysis.summary.insights.performThru}, Simple: ${analysis.summary.insights.performSimple || 0}, Varying: ${analysis.summary.insights.performVarying || 0}` 
        },
        { type: 'Paragraphs Detected', value: analysis.summary.insights.paragraphs.toString() },
        { 
          type: 'File I/O Detected', 
          value: analysis.summary.insights.fileIO ? 
            `Yes (${analysis.summary.insights.dataAccess})` : 'No' 
        },
        { type: 'VSAM or SEQUENTIAL', value: analysis.summary.insights.dataAccess },
        { type: 'JCL Mapping', value: analysis.summary.insights.jclMapping ? 'Detected' : 'Not detected (no JCL uploaded)' }
      ],
      missingFiles: analysis.summary.missing,
      complexity: analysis.summary.complexity,
      estimatedEffort: calculateEffort(analysis.summary),
      summary: analysis.summary,
      files: analysis.files
    };

    res.json(response);
  } catch (error) {
    console.error('Analysis error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/analyze/job/:jobId
 * Get existing analysis results for a job
 */
router.get('/job/:jobId', async (req, res) => {
  try {
    const { jobId } = req.params;
    const analysisPath = path.join(__dirname, '../uploads', jobId, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found' });
    }

    const analysis = await fs.readJson(analysisPath);
    res.json(analysis);
  } catch (error) {
    console.error('Get analysis error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * POST /api/analyze/file
 * Analyze a single file and return detailed breakdown
 */
router.post('/file', async (req, res) => {
  try {
    const { jobId, fileName } = req.body;
    
    if (!jobId || !fileName) {
      return res.status(400).json({ error: 'Job ID and file name are required' });
    }

    const filePath = path.join(__dirname, '../uploads', jobId, fileName);
    
    if (!fs.existsSync(filePath)) {
      return res.status(404).json({ error: 'File not found' });
    }

    const parser = new CobolParser();
    const analysis = parser.analyzeFile(filePath);
    
    if (!analysis) {
      return res.status(500).json({ error: 'Failed to analyze file' });
    }

    // Add pseudo-code generation (simplified for now)
    const fileContent = fs.readFileSync(filePath, 'utf8');
    const pseudoCode = generatePseudoCode(fileContent, analysis);

    res.json({
      ...analysis,
      pseudoCode,
      content: fileContent
    });
  } catch (error) {
    console.error('File analysis error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * POST /api/analyze/convert
 * Convert COBOL to target language (Java/Python)
 */
router.post('/convert', async (req, res) => {
  try {
    const { jobId, targetLanguage = 'java', files } = req.body;
    
    if (!jobId) {
      return res.status(400).json({ error: 'Job ID is required' });
    }

    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found. Please run analysis first.' });
    }

    const analysis = await fs.readJson(analysisPath);
    
    // Generate conversion recommendations
    const conversionPlan = generateConversionPlan(analysis, targetLanguage);
    
    // Save conversion plan
    const conversionPath = path.join(jobDir, `conversion_${targetLanguage}.json`);
    await fs.writeJson(conversionPath, conversionPlan);

    res.json({
      jobId,
      targetLanguage,
      conversionPlan,
      estimatedLines: Math.floor(analysis.summary.totalLOC * getConversionRatio(targetLanguage)),
      recommendations: generateRecommendations(analysis.summary, targetLanguage)
    });
  } catch (error) {
    console.error('Conversion error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * Helper function to calculate effort estimation
 */
function calculateEffort(summary) {
  const { totalLOC, complexity, missing } = summary;
  let days = 1;
  
  // Base effort on LOC
  if (totalLOC > 1000) days = 10;
  else if (totalLOC > 500) days = 5;
  else if (totalLOC > 200) days = 3;
  else days = 1;
  
  // Adjust for complexity
  if (complexity === 'HIGH') days *= 1.5;
  else if (complexity === 'MEDIUM') days *= 1.2;
  
  // Add time for missing dependencies
  days += missing.length * 0.5;
  
  const roundedDays = Math.ceil(days);
  
  if (roundedDays === 1) return '1 Day';
  if (roundedDays <= 3) return '2-3 Days';
  if (roundedDays <= 7) return '1 Week';
  if (roundedDays <= 14) return '2 Weeks';
  return `${Math.ceil(roundedDays / 7)} Weeks`;
}

/**
 * Generate pseudo-code from COBOL content
 */
function generatePseudoCode(content, analysis) {
  const lines = content.split('\n');
  const pseudoCode = [];
  
  // Simple pseudo-code generation based on structure
  analysis.structure.paragraphs.forEach(paragraph => {
    pseudoCode.push(`Function: ${paragraph}`);
    
    // Look for the paragraph in content and extract logic
    const paragraphRegex = new RegExp(`^\\s{0,3}${paragraph}\\s*\\.`, 'm');
    const match = content.match(paragraphRegex);
    
    if (match) {
      pseudoCode.push(`  - Executes ${paragraph} logic`);
      pseudoCode.push(`  - Contains business rules and data processing`);
    }
  });
  
  // Add dependency information
  if (analysis.dependencies.copy.length > 0) {
    pseudoCode.push('\nData Structures:');
    analysis.dependencies.copy.forEach(copy => {
      pseudoCode.push(`  - Includes ${copy} data definitions`);
    });
  }
  
  if (analysis.dependencies.call.length > 0) {
    pseudoCode.push('\nExternal Calls:');
    analysis.dependencies.call.forEach(call => {
      pseudoCode.push(`  - Calls external program ${call}`);
    });
  }
  
  return pseudoCode.join('\n');
}

/**
 * Generate conversion plan for target language
 */
function generateConversionPlan(analysis, targetLanguage) {
  const plan = {
    language: targetLanguage,
    strategy: 'incremental',
    phases: []
  };
  
  // Phase 1: Data Structure Conversion
  plan.phases.push({
    phase: 1,
    name: 'Data Structure Migration',
    description: `Convert COBOL data definitions to ${targetLanguage} classes/structures`,
    files: analysis.summary.copybooks,
    estimatedDays: analysis.summary.copybooks.length * 0.5
  });
  
  // Phase 2: Business Logic Conversion
  plan.phases.push({
    phase: 2,
    name: 'Business Logic Migration',
    description: `Convert COBOL procedures to ${targetLanguage} methods`,
    files: analysis.summary.programs,
    estimatedDays: Math.ceil(analysis.summary.totalLOC / 100)
  });
  
  // Phase 3: Integration and Testing
  plan.phases.push({
    phase: 3,
    name: 'Integration & Testing',
    description: 'Connect components and implement comprehensive testing',
    files: ['Unit Tests', 'Integration Tests'],
    estimatedDays: Math.ceil(analysis.summary.totalLOC / 200)
  });
  
  return plan;
}

/**
 * Get conversion ratio for different languages
 */
function getConversionRatio(language) {
  const ratios = {
    java: 1.3,    // Java typically requires more lines
    python: 0.7,  // Python is more concise
    csharp: 1.2,  // C# similar to Java
    javascript: 0.8
  };
  
  return ratios[language.toLowerCase()] || 1.0;
}

/**
 * Generate modernization recommendations
 */
function generateRecommendations(summary, targetLanguage) {
  const recommendations = [];
  
  // Language-specific recommendations
  if (targetLanguage === 'java') {
    recommendations.push('Use Spring Boot for enterprise application structure');
    recommendations.push('Implement JPA for data access layer');
    recommendations.push('Use Maven or Gradle for dependency management');
  } else if (targetLanguage === 'python') {
    recommendations.push('Use Django or Flask for web applications');
    recommendations.push('Implement SQLAlchemy for database operations');
    recommendations.push('Use pytest for comprehensive testing');
  }
  
  // Complexity-based recommendations
  if (summary.complexity === 'HIGH') {
    recommendations.push('Consider breaking down into microservices');
    recommendations.push('Implement comprehensive logging and monitoring');
    recommendations.push('Plan for phased rollout with parallel systems');
  }
  
  // Missing dependencies recommendations
  if (summary.missing.length > 0) {
    recommendations.push('Resolve missing dependencies before conversion');
    recommendations.push('Create stub implementations for missing components');
  }
  
  // File I/O recommendations
  if (summary.insights.fileIO) {
    recommendations.push('Replace file-based operations with database interactions');
    recommendations.push('Implement modern data access patterns');
  }
  
  return recommendations;
}

module.exports = router; 