const express = require('express');
const path = require('path');
const fs = require('fs-extra');

const router = express.Router();

/**
 * GET /api/export/metadata/:jobId
 * Export analysis metadata as JSON
 */
router.get('/metadata/:jobId', async (req, res) => {
  try {
    const { jobId } = req.params;
    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found' });
    }

    const analysis = await fs.readJson(analysisPath);
    
    const metadata = {
      jobId,
      timestamp: new Date().toISOString(),
      version: '1.0.0',
      analysis: analysis.summary,
      files: analysis.files.map(f => ({
        name: f.fileName,
        type: f.type,
        loc: f.loc,
        dependencies: f.dependencies,
        complexity: f.patterns
      })),
      recommendations: generateExportRecommendations(analysis.summary),
      auditTrail: {
        analyzedAt: new Date().toISOString(),
        tool: 'COBOL Flow Hub',
        version: '1.0.0'
      }
    };

    // Set headers for file download
    res.setHeader('Content-Type', 'application/json');
    res.setHeader('Content-Disposition', `attachment; filename="cobol-analysis-${jobId}.json"`);
    
    res.json(metadata);
  } catch (error) {
    console.error('Export metadata error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/export/report/:jobId
 * Generate and export comprehensive analysis report
 */
router.get('/report/:jobId', async (req, res) => {
  try {
    const { jobId } = req.params;
    const { format = 'json' } = req.query;
    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found' });
    }

    const analysis = await fs.readJson(analysisPath);
    
    const report = generateDetailedReport(analysis, jobId);
    
    if (format === 'csv') {
      const csv = convertToCSV(report);
      res.setHeader('Content-Type', 'text/csv');
      res.setHeader('Content-Disposition', `attachment; filename="cobol-report-${jobId}.csv"`);
      res.send(csv);
    } else if (format === 'txt') {
      const text = convertToText(report);
      res.setHeader('Content-Type', 'text/plain');
      res.setHeader('Content-Disposition', `attachment; filename="cobol-report-${jobId}.txt"`);
      res.send(text);
    } else {
      res.setHeader('Content-Type', 'application/json');
      res.setHeader('Content-Disposition', `attachment; filename="cobol-report-${jobId}.json"`);
      res.json(report);
    }
  } catch (error) {
    console.error('Export report error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/export/mapping/:jobId
 * Export COBOL to modern language mapping table
 */
router.get('/mapping/:jobId', async (req, res) => {
  try {
    const { jobId } = req.params;
    const { targetLanguage = 'java' } = req.query;
    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found' });
    }

    const analysis = await fs.readJson(analysisPath);
    const mappingTable = generateMappingTable(analysis, targetLanguage);
    
    res.setHeader('Content-Type', 'application/json');
    res.setHeader('Content-Disposition', `attachment; filename="cobol-${targetLanguage}-mapping-${jobId}.json"`);
    
    res.json(mappingTable);
  } catch (error) {
    console.error('Export mapping error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * POST /api/export/custom
 * Generate custom export with specific sections
 */
router.post('/custom', async (req, res) => {
  try {
    const { jobId, sections = [], format = 'json', includeSourceCode = false } = req.body;
    
    if (!jobId) {
      return res.status(400).json({ error: 'Job ID is required' });
    }

    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found' });
    }

    const analysis = await fs.readJson(analysisPath);
    const customReport = generateCustomReport(analysis, sections, includeSourceCode, jobDir);
    
    const timestamp = new Date().toISOString().slice(0, 19).replace(/[:.]/g, '-');
    const filename = `cobol-custom-export-${jobId}-${timestamp}`;
    
    if (format === 'csv') {
      const csv = convertToCSV(customReport);
      res.setHeader('Content-Type', 'text/csv');
      res.setHeader('Content-Disposition', `attachment; filename="${filename}.csv"`);
      res.send(csv);
    } else {
      res.setHeader('Content-Type', 'application/json');
      res.setHeader('Content-Disposition', `attachment; filename="${filename}.json"`);
      res.json(customReport);
    }
  } catch (error) {
    console.error('Custom export error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * Generate export recommendations
 */
function generateExportRecommendations(summary) {
  const recommendations = [];
  
  if (summary.missing.length > 0) {
    recommendations.push({
      type: 'CRITICAL',
      category: 'Missing Dependencies',
      description: `${summary.missing.length} missing dependencies detected`,
      action: 'Resolve missing files before modernization',
      files: summary.missing
    });
  }
  
  if (summary.complexity === 'HIGH') {
    recommendations.push({
      type: 'WARNING',
      category: 'High Complexity',
      description: 'High complexity codebase detected',
      action: 'Consider phased approach or additional resources',
      impact: 'Extended timeline and increased effort required'
    });
  }
  
  if (summary.insights.performThru > 5) {
    recommendations.push({
      type: 'INFO',
      category: 'PERFORM THRU Pattern',
      description: `${summary.insights.performThru} PERFORM THRU statements found`,
      action: 'Review for potential refactoring to modern control structures',
      modernizationNote: 'Convert to structured programming patterns'
    });
  }
  
  if (summary.insights.fileIO) {
    recommendations.push({
      type: 'INFO',
      category: 'File I/O Operations',
      description: 'File I/O operations detected',
      action: 'Plan database migration strategy',
      modernizationNote: 'Replace file operations with database transactions'
    });
  }
  
  return recommendations;
}

/**
 * Generate detailed analysis report
 */
function generateDetailedReport(analysis, jobId) {
  return {
    reportInfo: {
      jobId,
      generatedAt: new Date().toISOString(),
      tool: 'COBOL Flow Hub',
      version: '1.0.0'
    },
    executiveSummary: {
      totalFiles: analysis.files.length,
      totalLOC: analysis.summary.totalLOC,
      complexity: analysis.summary.complexity,
      criticalIssues: analysis.summary.missing.length,
      estimatedEffort: calculateEffortDays(analysis.summary)
    },
    fileAnalysis: analysis.files.map(file => ({
      fileName: file.fileName,
      type: file.type,
      linesOfCode: file.loc,
      dependencies: {
        copyStatements: file.dependencies.copy.length,
        callStatements: file.dependencies.call.length,
        total: file.dependencies.copy.length + file.dependencies.call.length
      },
      complexity: {
        performThru: file.patterns.performThru,
        conditionalLogic: file.patterns.conditionalLogic,
        goToStatements: file.patterns.goToStatements
      },
      structure: {
        paragraphs: file.structure.paragraphs.length,
        hasFileIO: file.structure.fileIO,
        dataAccess: file.structure.dataAccess
      }
    })),
    dependencyAnalysis: {
      totalDependencies: analysis.summary.dependencies.copy.length + analysis.summary.dependencies.call.length,
      copyDependencies: analysis.summary.dependencies.copy,
      callDependencies: analysis.summary.dependencies.call,
      missingDependencies: analysis.summary.missing,
      dependencyMatrix: generateDependencyMatrix(analysis.files)
    },
    modernizationReadiness: {
      score: calculateModernizationScore(analysis.summary),
      blockers: analysis.summary.missing,
      recommendations: generateExportRecommendations(analysis.summary)
    }
  };
}

/**
 * Generate COBOL to modern language mapping table
 */
function generateMappingTable(analysis, targetLanguage) {
  const mappings = [];
  
  analysis.files.forEach(file => {
    file.structure.paragraphs.forEach(paragraph => {
      mappings.push({
        cobolElement: paragraph,
        elementType: 'PARAGRAPH',
        cobolFile: file.fileName,
        targetLanguage: targetLanguage,
        suggestedMapping: `${paragraph.toLowerCase().replace(/-/g, '_')}()`,
        mappingType: 'METHOD',
        complexity: 'MEDIUM',
        notes: 'Convert paragraph to method with appropriate parameters'
      });
    });
    
    file.dependencies.copy.forEach(copybook => {
      mappings.push({
        cobolElement: copybook,
        elementType: 'COPYBOOK',
        cobolFile: file.fileName,
        targetLanguage: targetLanguage,
        suggestedMapping: targetLanguage === 'java' ? 
          `${copybook}DTO.java` : `${copybook.toLowerCase()}.py`,
        mappingType: 'DATA_STRUCTURE',
        complexity: 'LOW',
        notes: 'Convert copybook to data transfer object or class'
      });
    });
    
    file.dependencies.call.forEach(call => {
      mappings.push({
        cobolElement: call,
        elementType: 'EXTERNAL_CALL',
        cobolFile: file.fileName,
        targetLanguage: targetLanguage,
        suggestedMapping: targetLanguage === 'java' ? 
          `${call}Service.java` : `${call.toLowerCase()}_service.py`,
        mappingType: 'SERVICE_CLASS',
        complexity: 'HIGH',
        notes: 'Implement as service class with equivalent functionality'
      });
    });
  });
  
  return {
    jobId: analysis.jobId,
    targetLanguage,
    totalMappings: mappings.length,
    mappings,
    conversionGuidelines: getConversionGuidelines(targetLanguage)
  };
}

/**
 * Generate custom report based on selected sections
 */
function generateCustomReport(analysis, sections, includeSourceCode, jobDir) {
  const report = {
    metadata: {
      jobId: analysis.jobId || 'unknown',
      timestamp: new Date().toISOString(),
      sections: sections
    }
  };
  
  if (sections.includes('summary') || sections.length === 0) {
    report.summary = analysis.summary;
  }
  
  if (sections.includes('files') || sections.length === 0) {
    report.files = analysis.files;
  }
  
  if (sections.includes('dependencies')) {
    report.dependencies = {
      copy: analysis.summary.dependencies.copy,
      call: analysis.summary.dependencies.call,
      missing: analysis.summary.missing,
      matrix: generateDependencyMatrix(analysis.files)
    };
  }
  
  if (sections.includes('complexity')) {
    report.complexity = {
      overall: analysis.summary.complexity,
      fileBreakdown: analysis.files.map(f => ({
        file: f.fileName,
        loc: f.loc,
        patterns: f.patterns
      }))
    };
  }
  
  if (includeSourceCode) {
    report.sourceCode = {};
    analysis.files.forEach(file => {
      try {
        const filePath = path.join(jobDir, file.fileName);
        if (fs.existsSync(filePath)) {
          report.sourceCode[file.fileName] = fs.readFileSync(filePath, 'utf8');
        }
      } catch (error) {
        console.error(`Error reading source file ${file.fileName}:`, error);
      }
    });
  }
  
  return report;
}

/**
 * Helper functions for conversions
 */
function convertToCSV(data) {
  // Simple CSV conversion for file analysis
  const headers = 'File,Type,LOC,Copy Dependencies,Call Dependencies,Complexity\n';
  let rows = '';
  
  if (data.fileAnalysis) {
    data.fileAnalysis.forEach(file => {
      rows += `"${file.fileName}","${file.type}",${file.linesOfCode},${file.dependencies.copyStatements},${file.dependencies.callStatements},"${file.complexity}"\n`;
    });
  }
  
  return headers + rows;
}

function convertToText(data) {
  let text = `COBOL Analysis Report\n`;
  text += `===================\n\n`;
  text += `Generated: ${new Date().toLocaleString()}\n`;
  text += `Job ID: ${data.reportInfo?.jobId || 'Unknown'}\n\n`;
  
  if (data.executiveSummary) {
    text += `Executive Summary:\n`;
    text += `- Total Files: ${data.executiveSummary.totalFiles}\n`;
    text += `- Total Lines of Code: ${data.executiveSummary.totalLOC}\n`;
    text += `- Complexity: ${data.executiveSummary.complexity}\n`;
    text += `- Critical Issues: ${data.executiveSummary.criticalIssues}\n\n`;
  }
  
  return text;
}

function generateDependencyMatrix(files) {
  const matrix = {};
  
  files.forEach(file => {
    matrix[file.fileName] = {
      dependsOn: [...file.dependencies.copy, ...file.dependencies.call],
      dependedBy: []
    };
  });
  
  // Calculate reverse dependencies
  Object.keys(matrix).forEach(fileName => {
    matrix[fileName].dependsOn.forEach(dep => {
      if (matrix[dep]) {
        matrix[dep].dependedBy.push(fileName);
      }
    });
  });
  
  return matrix;
}

function calculateEffortDays(summary) {
  let days = Math.ceil(summary.totalLOC / 100);
  if (summary.complexity === 'HIGH') days *= 1.5;
  if (summary.missing.length > 0) days += summary.missing.length;
  return Math.max(1, days);
}

function calculateModernizationScore(summary) {
  let score = 100;
  
  // Deduct for complexity
  if (summary.complexity === 'HIGH') score -= 30;
  else if (summary.complexity === 'MEDIUM') score -= 15;
  
  // Deduct for missing dependencies
  score -= summary.missing.length * 10;
  
  // Deduct for high PERFORM THRU usage
  score -= Math.min(summary.insights.performThru * 2, 20);
  
  return Math.max(0, score);
}

function getConversionGuidelines(targetLanguage) {
  const guidelines = {
    java: [
      'Use Spring Boot for enterprise applications',
      'Implement JPA for data persistence',
      'Convert PERFORM to method calls',
      'Use Maven or Gradle for build management',
      'Implement proper exception handling'
    ],
    python: [
      'Use Django or Flask for web applications',
      'Implement SQLAlchemy for ORM',
      'Convert paragraphs to functions',
      'Use pip and requirements.txt for dependencies',
      'Follow PEP 8 style guidelines'
    ],
    csharp: [
      'Use .NET Core for cross-platform compatibility',
      'Implement Entity Framework for data access',
      'Convert to object-oriented design',
      'Use NuGet for package management',
      'Implement async/await patterns'
    ]
  };
  
  return guidelines[targetLanguage.toLowerCase()] || guidelines.java;
}

module.exports = router; 