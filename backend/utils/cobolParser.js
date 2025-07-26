const fs = require('fs');
const path = require('path');

class CobolParser {
  constructor() {
    this.analysisResults = {
      programs: [],
      copybooks: [],
      totalLOC: 0,
      dependencies: {
        copy: [],
        call: []
      },
      missing: [],
      insights: {
        performThru: 0,
        performSimple: 0,
        performVarying: 0,
        paragraphs: 0,
        fileIO: false,
        dataAccess: 'UNKNOWN',
        jclMapping: false
      },
      complexity: 'LOW'
    };
  }

  /**
   * Analyze a single COBOL file
   */
  analyzeFile(filePath) {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const fileName = path.basename(filePath);
      const extension = path.extname(fileName).toLowerCase();
      
      const analysis = {
        fileName,
        filePath,
        type: this.determineFileType(fileName, extension),
        loc: this.countLinesOfCode(content),
        dependencies: this.extractDependencies(content),
        structure: this.analyzeStructure(content),
        patterns: this.detectPatterns(content)
      };

      return analysis;
    } catch (error) {
      console.error(`Error analyzing file ${filePath}:`, error);
      return null;
    }
  }

  /**
   * Analyze multiple COBOL files and generate comprehensive report
   */
  analyzeDirectory(dirPath) {
    const files = fs.readdirSync(dirPath);
    const cobolFiles = files.filter(file => 
      /\.(cob|cbl|cobol|cpy|copy|inc)$/i.test(file)
    );

    this.analysisResults = {
      programs: [],
      copybooks: [],
      totalLOC: 0,
      dependencies: { copy: [], call: [] },
      missing: [],
      insights: {
        performThru: 0,
        performSimple: 0,
        performVarying: 0,
        paragraphs: 0,
        fileIO: false,
        dataAccess: 'UNKNOWN',
        jclMapping: false
      },
      complexity: 'LOW'
    };

    const fileAnalyses = [];

    for (const file of cobolFiles) {
      const filePath = path.join(dirPath, file);
      const analysis = this.analyzeFile(filePath);
      
      if (analysis) {
        fileAnalyses.push(analysis);
        this.updateGlobalResults(analysis);
      }
    }

    // Detect missing dependencies
    this.detectMissingDependencies(fileAnalyses, cobolFiles);
    
    // Calculate complexity
    this.calculateComplexity();

    return {
      summary: this.analysisResults,
      files: fileAnalyses
    };
  }

  /**
   * Determine if file is a program or copybook
   */
  determineFileType(fileName, extension) {
    const copyExtensions = ['.cpy', '.copy', '.inc'];
    const programExtensions = ['.cob', '.cbl', '.cobol'];
    
    if (copyExtensions.includes(extension)) {
      return 'copybook';
    } else if (programExtensions.includes(extension)) {
      return 'program';
    }
    
    // Analyze content for additional clues
    return 'program'; // Default assumption
  }

  /**
   * Count meaningful lines of code (excluding comments and blank lines)
   */
  countLinesOfCode(content) {
    const lines = content.split('\n');
    let loc = 0;
    
    for (const line of lines) {
      const trimmed = line.trim();
      // Skip blank lines and comment lines (starting with * in column 7)
      if (trimmed && !trimmed.startsWith('*') && trimmed !== '') {
        // Check if it's a comment line (column 7 = *)
        if (line.length > 6 && line[6] !== '*') {
          loc++;
        }
      }
    }
    
    return loc;
  }

  /**
   * Extract COPY and CALL dependencies
   */
  extractDependencies(content) {
    const dependencies = { copy: [], call: [] };
    
    // Extract COPY statements
    const copyRegex = /COPY\s+['"]*([A-Za-z0-9\-_]+)['"]*\s*\.?/gi;
    let match;
    
    while ((match = copyRegex.exec(content)) !== null) {
      // Clean up the name - remove any trailing periods or quotes
      const copyName = match[1].replace(/[.'"]+$/, '').trim();
      if (copyName && !dependencies.copy.includes(copyName)) {
        dependencies.copy.push(copyName);
      }
    }
    
    // Extract CALL statements
    const callRegex = /CALL\s+['"]+([A-Za-z0-9\-_]+)['"]+/gi;
    
    while ((match = callRegex.exec(content)) !== null) {
      // Clean up the name - remove any quotes
      const callName = match[1].replace(/['"]+/g, '').trim();
      if (callName && !dependencies.call.includes(callName)) {
        dependencies.call.push(callName);
      }
    }
    
    console.log('Extracted dependencies:', dependencies);
    return dependencies;
  }

  /**
   * Analyze COBOL program structure
   */
  analyzeStructure(content) {
    const structure = {
      divisions: [],
      paragraphs: [],
      sections: [],
      fileIO: false,
      dataAccess: 'UNKNOWN'
    };

    // Count paragraphs (lines ending with . and starting in area A - columns 8-11)
    // COBOL paragraphs can start anywhere from column 8-11 (7-10 zero-indexed)
    const paragraphRegex = /^\s{4,11}[A-Za-z][A-Za-z0-9\-]*\s*\./gm;
    const paragraphs = content.match(paragraphRegex) || [];
    structure.paragraphs = paragraphs.map(p => p.trim().replace('.', ''));

    // Detect divisions
    const divisionRegex = /(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION/gi;
    const divisions = content.match(divisionRegex) || [];
    structure.divisions = divisions.map(d => d.toUpperCase());

    // Detect file I/O
    const fileIORegex = /(OPEN\s+(INPUT|OUTPUT|I-O)|READ\s+|WRITE\s+|CLOSE\s+)/gi;
    structure.fileIO = fileIORegex.test(content);

    // Detect data access type
    if (content.includes('VSAM') || content.includes('KSDS') || content.includes('ESDS')) {
      structure.dataAccess = 'VSAM';
    } else if (content.includes('SEQUENTIAL') || structure.fileIO) {
      structure.dataAccess = 'SEQUENTIAL';
    }

    return structure;
  }

  /**
   * Detect specific COBOL patterns
   */
  detectPatterns(content) {
    const patterns = {
      performThru: 0,
      performUntil: 0,
      performSimple: 0,
      performVarying: 0,
      nestedPerforms: 0,
      goToStatements: 0,
      conditionalLogic: 0
    };

    // Count PERFORM THRU statements
    const performThruRegex = /PERFORM\s+[A-Za-z0-9\-_.]+\s+THRU\s+[A-Za-z0-9\-_.]+/gi;
    patterns.performThru = (content.match(performThruRegex) || []).length;

    // Count PERFORM UNTIL statements
    const performUntilRegex = /PERFORM\s+.*UNTIL\s+/gi;
    patterns.performUntil = (content.match(performUntilRegex) || []).length;

    // Count PERFORM VARYING statements
    const performVaryingRegex = /PERFORM\s+VARYING\s+/gi;
    patterns.performVarying = (content.match(performVaryingRegex) || []).length;

    // Count simple PERFORM statements (excluding THRU, UNTIL, VARYING)
    const performSimpleRegex = /PERFORM\s+[A-Za-z0-9\-_.]+(?!\s+(THRU|UNTIL|VARYING))/gi;
    patterns.performSimple = (content.match(performSimpleRegex) || []).length;

    // Count GO TO statements
    const goToRegex = /GO\s+TO\s+[A-Za-z0-9\-_.]+/gi;
    patterns.goToStatements = (content.match(goToRegex) || []).length;

    // Count conditional statements
    const conditionalRegex = /(IF\s+|EVALUATE\s+|WHEN\s+)/gi;
    patterns.conditionalLogic = (content.match(conditionalRegex) || []).length;

    return patterns;
  }

  /**
   * Update global analysis results
   */
  updateGlobalResults(fileAnalysis) {
    if (fileAnalysis.type === 'program') {
      this.analysisResults.programs.push(fileAnalysis.fileName);
    } else {
      this.analysisResults.copybooks.push(fileAnalysis.fileName);
    }

    this.analysisResults.totalLOC += fileAnalysis.loc;

    // Merge dependencies
    fileAnalysis.dependencies.copy.forEach(dep => {
      if (!this.analysisResults.dependencies.copy.includes(dep)) {
        this.analysisResults.dependencies.copy.push(dep);
      }
    });

    fileAnalysis.dependencies.call.forEach(dep => {
      if (!this.analysisResults.dependencies.call.includes(dep)) {
        this.analysisResults.dependencies.call.push(dep);
      }
    });

    // Update insights
    this.analysisResults.insights.performThru += fileAnalysis.patterns.performThru;
    this.analysisResults.insights.performSimple = (this.analysisResults.insights.performSimple || 0) + fileAnalysis.patterns.performSimple;
    this.analysisResults.insights.performVarying = (this.analysisResults.insights.performVarying || 0) + fileAnalysis.patterns.performVarying;
    this.analysisResults.insights.paragraphs += fileAnalysis.structure.paragraphs.length;
    
    if (fileAnalysis.structure.fileIO) {
      this.analysisResults.insights.fileIO = true;
    }
    
    if (fileAnalysis.structure.dataAccess !== 'UNKNOWN') {
      this.analysisResults.insights.dataAccess = fileAnalysis.structure.dataAccess;
    }
  }

  /**
   * Detect missing dependencies
   */
  detectMissingDependencies(fileAnalyses, availableFiles) {
    const missing = new Set();
    
    console.log('Checking for missing dependencies...');
    console.log('Available files:', availableFiles);
    console.log('COPY dependencies to check:', this.analysisResults.dependencies.copy);
    console.log('CALL dependencies to check:', this.analysisResults.dependencies.call);
    
    // Check for missing COPY files
    this.analysisResults.dependencies.copy.forEach(copyName => {
      const possibleNames = [
        copyName,
        `${copyName}.cpy`,
        `${copyName}.copy`,
        `${copyName}.inc`
      ];
      
      const found = possibleNames.some(name => 
        availableFiles.some(file => 
          file.toLowerCase() === name.toLowerCase()
        )
      );
      
      console.log(`COPY ${copyName}: possible names = ${possibleNames}, found = ${found}`);
      
      if (!found) {
        missing.add(copyName);
      }
    });

    // Check for missing CALL targets (external programs)
    this.analysisResults.dependencies.call.forEach(callName => {
      const possibleNames = [
        callName,
        `${callName}.cbl`,
        `${callName}.cob`,
        `${callName}.cobol`
      ];
      
      const found = possibleNames.some(name => 
        availableFiles.some(file => 
          file.toLowerCase() === name.toLowerCase()
        )
      );
      
      console.log(`CALL ${callName}: possible names = ${possibleNames}, found = ${found}`);
      
      if (!found) {
        missing.add(callName);
      }
    });

    console.log('Missing dependencies found:', Array.from(missing));
    this.analysisResults.missing = Array.from(missing);
  }

  /**
   * Calculate complexity score based on various factors
   */
  calculateComplexity() {
    let score = 0;
    
    // LOC factor
    if (this.analysisResults.totalLOC > 500) score += 3;
    else if (this.analysisResults.totalLOC > 200) score += 2;
    else score += 1;
    
    // Dependencies factor
    const totalDeps = this.analysisResults.dependencies.copy.length + 
                     this.analysisResults.dependencies.call.length;
    if (totalDeps > 10) score += 3;
    else if (totalDeps > 5) score += 2;
    else score += 1;
    
    // Missing dependencies penalty
    score += this.analysisResults.missing.length;
    
    // PERFORM THRU complexity
    if (this.analysisResults.insights.performThru > 5) score += 2;
    else if (this.analysisResults.insights.performThru > 0) score += 1;
    
    // Determine complexity level
    if (score <= 3) this.analysisResults.complexity = 'LOW';
    else if (score <= 7) this.analysisResults.complexity = 'MEDIUM';
    else this.analysisResults.complexity = 'HIGH';
  }

  /**
   * Generate dependency graph data for ReactFlow
   */
  generateGraphData(fileAnalyses) {
    const nodes = [];
    const edges = [];
    const nodePositions = this.calculateNodePositions(fileAnalyses);
    
    // Create nodes for each file
    fileAnalyses.forEach((file, index) => {
      nodes.push({
        id: file.fileName,
        type: 'default',
        position: nodePositions[index],
        data: { label: file.fileName },
        style: this.getNodeStyle(file.type, false)
      });
    });
    
    // Create nodes for missing dependencies
    console.log('Missing dependencies for graph:', this.analysisResults.missing);
    this.analysisResults.missing.forEach((missing, index) => {
      const position = {
        x: 100 + (index * 150),
        y: 300
      };
      
      console.log(`Creating missing node: ${missing} at position (${position.x}, ${position.y})`);
      nodes.push({
        id: missing,
        type: 'default',
        position,
        data: { label: missing },
        style: this.getNodeStyle('missing', true)
      });
    });
    
    // Create edges for dependencies
    fileAnalyses.forEach(file => {
      // COPY dependencies
      file.dependencies.copy.forEach(copyName => {
        edges.push({
          id: `${file.fileName}-copy-${copyName}`,
          source: file.fileName,
          target: copyName,
          type: 'straight',
          style: { stroke: 'hsl(var(--primary))', strokeWidth: 2 },
          label: 'COPY',
          labelStyle: { fontSize: '10px', fontWeight: '500' }
        });
      });
      
      // CALL dependencies
      file.dependencies.call.forEach(callName => {
        edges.push({
          id: `${file.fileName}-call-${callName}`,
          source: file.fileName,
          target: callName,
          type: 'straight',
          style: { 
            stroke: 'hsl(var(--primary))', 
            strokeWidth: 2, 
            strokeDasharray: '5,5' 
          },
          label: 'CALL',
          labelStyle: { fontSize: '10px', fontWeight: '500' }
        });
      });
    });
    
    console.log('Generated graph data:');
    console.log('Nodes:', nodes.map(n => ({ id: n.id, label: n.data.label, isMissing: n.style?.border?.includes('destructive') })));
    console.log('Edges:', edges.map(e => ({ id: e.id, source: e.source, target: e.target, label: e.label })));
    
    return { nodes, edges };
  }

  /**
   * Calculate node positions for graph layout
   */
  calculateNodePositions(fileAnalyses) {
    const positions = [];
    const centerX = 250;
    const centerY = 100;
    const radius = 150;
    
    fileAnalyses.forEach((file, index) => {
      if (index === 0) {
        // Main program in center
        positions.push({ x: centerX, y: centerY });
      } else {
        // Arrange others in circle
        const angle = (2 * Math.PI * (index - 1)) / (fileAnalyses.length - 1);
        positions.push({
          x: centerX + radius * Math.cos(angle),
          y: centerY + radius * Math.sin(angle)
        });
      }
    });
    
    return positions;
  }

  /**
   * Get node styling based on type
   */
  getNodeStyle(type, isMissing) {
    if (isMissing) {
      return {
        background: 'hsl(var(--background))',
        color: 'hsl(var(--foreground))',
        border: '2px dashed hsl(var(--destructive))',
        borderRadius: '8px',
        fontSize: '12px',
        fontWeight: '500'
      };
    }
    
    switch (type) {
      case 'program':
        return {
          background: 'hsl(var(--primary))',
          color: 'hsl(var(--primary-foreground))',
          border: '2px solid hsl(var(--primary))',
          borderRadius: '8px',
          fontSize: '12px',
          fontWeight: '500'
        };
      case 'copybook':
        return {
          background: 'hsl(var(--secondary))',
          color: 'hsl(var(--secondary-foreground))',
          border: '2px solid hsl(var(--secondary))',
          borderRadius: '8px',
          fontSize: '12px',
          fontWeight: '500'
        };
      default:
        return {
          background: 'hsl(var(--background))',
          color: 'hsl(var(--foreground))',
          border: '2px solid hsl(var(--border))',
          borderRadius: '8px',
          fontSize: '12px',
          fontWeight: '500'
        };
    }
  }
}

module.exports = CobolParser; 