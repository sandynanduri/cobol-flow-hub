const express = require('express');
const path = require('path');
const fs = require('fs-extra');
const CobolParser = require('../utils/cobolParser');

const router = express.Router();

/**
 * GET /api/graph/:jobId
 * Generate dependency graph data for ReactFlow
 */
router.get('/:jobId', async (req, res) => {
  try {
    const { jobId } = req.params;
    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found. Please run analysis first.' });
    }

    const analysis = await fs.readJson(analysisPath);
    const parser = new CobolParser();
    
    // Restore the analysis results to the parser
    parser.analysisResults = analysis.summary;
    
    // Generate graph data
    const graphData = parser.generateGraphData(analysis.files);
    
    // Save graph data for caching
    const graphPath = path.join(jobDir, 'graph.json');
    await fs.writeJson(graphPath, graphData);

    res.json({
      jobId,
      timestamp: new Date().toISOString(),
      ...graphData,
      metadata: {
        totalNodes: graphData.nodes.length,
        totalEdges: graphData.edges.length,
        missingNodes: graphData.nodes.filter(n => n.style?.border?.includes('destructive')).length
      }
    });
  } catch (error) {
    console.error('Graph generation error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * POST /api/graph/custom
 * Generate custom graph with specific layout options
 */
router.post('/custom', async (req, res) => {
  try {
    const { jobId, layout = 'hierarchical', showMissing = true, groupByType = false } = req.body;
    
    if (!jobId) {
      return res.status(400).json({ error: 'Job ID is required' });
    }

    const jobDir = path.join(__dirname, '../uploads', jobId);
    const analysisPath = path.join(jobDir, 'analysis.json');
    
    if (!fs.existsSync(analysisPath)) {
      return res.status(404).json({ error: 'Analysis not found' });
    }

    const analysis = await fs.readJson(analysisPath);
    const parser = new CobolParser();
    
    // Restore the analysis results to the parser
    parser.analysisResults = analysis.summary;
    
    let graphData = parser.generateGraphData(analysis.files);
    
    // Apply custom layout
    graphData = applyCustomLayout(graphData, layout, showMissing, groupByType);
    
    res.json({
      jobId,
      layout,
      showMissing,
      groupByType,
      ...graphData
    });
  } catch (error) {
    console.error('Custom graph error:', error);
    res.status(500).json({ error: error.message });
  }
});

/**
 * Apply custom layout algorithms
 */
function applyCustomLayout(graphData, layout, showMissing, groupByType) {
  let { nodes, edges } = graphData;
  
  // Filter out missing nodes if requested
  if (!showMissing) {
    const missingNodeIds = nodes
      .filter(n => n.style?.border?.includes('destructive'))
      .map(n => n.id);
    
    nodes = nodes.filter(n => !missingNodeIds.includes(n.id));
    edges = edges.filter(e => 
      !missingNodeIds.includes(e.source) && !missingNodeIds.includes(e.target)
    );
  }
  
  // Apply layout algorithm
  switch (layout) {
    case 'circular':
      nodes = applyCircularLayout(nodes);
      break;
    case 'grid':
      nodes = applyGridLayout(nodes);
      break;
    case 'force-directed':
      nodes = applyForceDirectedLayout(nodes, edges);
      break;
    case 'hierarchical':
    default:
      nodes = applyHierarchicalLayout(nodes, edges);
      break;
  }
  
  // Group by type if requested
  if (groupByType) {
    nodes = applyTypeGrouping(nodes);
  }
  
  return { nodes, edges };
}

/**
 * Layout algorithms
 */
function applyCircularLayout(nodes) {
  const centerX = 300;
  const centerY = 200;
  const radius = 150;
  
  return nodes.map((node, index) => ({
    ...node,
    position: {
      x: centerX + radius * Math.cos((2 * Math.PI * index) / nodes.length),
      y: centerY + radius * Math.sin((2 * Math.PI * index) / nodes.length)
    }
  }));
}

function applyGridLayout(nodes) {
  const cols = Math.ceil(Math.sqrt(nodes.length));
  const cellWidth = 200;
  const cellHeight = 100;
  
  return nodes.map((node, index) => ({
    ...node,
    position: {
      x: (index % cols) * cellWidth + 50,
      y: Math.floor(index / cols) * cellHeight + 50
    }
  }));
}

function applyHierarchicalLayout(nodes, edges) {
  // Find root nodes (nodes with no incoming edges)
  const targetNodes = new Set(edges.map(e => e.target));
  const rootNodes = nodes.filter(n => !targetNodes.has(n.id));
  
  // Place root nodes at top
  const levels = [];
  levels[0] = rootNodes;
  
  // Build hierarchy levels
  let currentLevel = 0;
  const processedNodes = new Set(rootNodes.map(n => n.id));
  
  while (levels[currentLevel] && levels[currentLevel].length > 0) {
    const nextLevel = [];
    
    levels[currentLevel].forEach(node => {
      const children = edges
        .filter(e => e.source === node.id)
        .map(e => nodes.find(n => n.id === e.target))
        .filter(n => n && !processedNodes.has(n.id));
      
      children.forEach(child => {
        if (!processedNodes.has(child.id)) {
          nextLevel.push(child);
          processedNodes.add(child.id);
        }
      });
    });
    
    if (nextLevel.length > 0) {
      levels[currentLevel + 1] = nextLevel;
      currentLevel++;
    } else {
      break;
    }
  }
  
  // Position nodes by level
  return nodes.map(node => {
    for (let level = 0; level < levels.length; level++) {
      const levelNodes = levels[level];
      const nodeIndex = levelNodes.findIndex(n => n.id === node.id);
      
      if (nodeIndex !== -1) {
        return {
          ...node,
          position: {
            x: (nodeIndex * 200) + 100 - (levelNodes.length * 50),
            y: level * 150 + 50
          }
        };
      }
    }
    
    // Fallback position for unprocessed nodes
    return {
      ...node,
      position: { x: Math.random() * 400 + 100, y: Math.random() * 200 + 100 }
    };
  });
}

function applyForceDirectedLayout(nodes, edges) {
  // Simplified force-directed layout
  const iterations = 100;
  const k = Math.sqrt(400 * 300 / nodes.length); // Optimal distance
  
  // Initialize random positions if not set
  nodes.forEach(node => {
    if (!node.position) {
      node.position = {
        x: Math.random() * 400 + 100,
        y: Math.random() * 300 + 100
      };
    }
  });
  
  // Apply forces
  for (let iter = 0; iter < iterations; iter++) {
    const forces = nodes.map(() => ({ x: 0, y: 0 }));
    
    // Repulsive forces between all nodes
    for (let i = 0; i < nodes.length; i++) {
      for (let j = i + 1; j < nodes.length; j++) {
        const dx = nodes[i].position.x - nodes[j].position.x;
        const dy = nodes[i].position.y - nodes[j].position.y;
        const distance = Math.sqrt(dx * dx + dy * dy) || 1;
        const force = k * k / distance;
        
        forces[i].x += (dx / distance) * force;
        forces[i].y += (dy / distance) * force;
        forces[j].x -= (dx / distance) * force;
        forces[j].y -= (dy / distance) * force;
      }
    }
    
    // Attractive forces for connected nodes
    edges.forEach(edge => {
      const sourceIndex = nodes.findIndex(n => n.id === edge.source);
      const targetIndex = nodes.findIndex(n => n.id === edge.target);
      
      if (sourceIndex !== -1 && targetIndex !== -1) {
        const dx = nodes[sourceIndex].position.x - nodes[targetIndex].position.x;
        const dy = nodes[sourceIndex].position.y - nodes[targetIndex].position.y;
        const distance = Math.sqrt(dx * dx + dy * dy) || 1;
        const force = distance * distance / k;
        
        forces[sourceIndex].x -= (dx / distance) * force;
        forces[sourceIndex].y -= (dy / distance) * force;
        forces[targetIndex].x += (dx / distance) * force;
        forces[targetIndex].y += (dy / distance) * force;
      }
    });
    
    // Apply forces and damping
    const damping = 0.9;
    nodes.forEach((node, i) => {
      node.position.x += forces[i].x * damping;
      node.position.y += forces[i].y * damping;
      
      // Keep nodes within bounds
      node.position.x = Math.max(50, Math.min(750, node.position.x));
      node.position.y = Math.max(50, Math.min(550, node.position.y));
    });
  }
  
  return nodes;
}

function applyTypeGrouping(nodes) {
  const groups = {
    program: { x: 100, y: 100 },
    copybook: { x: 400, y: 100 },
    missing: { x: 250, y: 300 }
  };
  
  const typeCount = { program: 0, copybook: 0, missing: 0 };
  
  return nodes.map(node => {
    let type = 'program';
    if (node.style?.border?.includes('destructive')) {
      type = 'missing';
    } else if (node.style?.background?.includes('secondary')) {
      type = 'copybook';
    }
    
    const groupPos = groups[type];
    const offset = typeCount[type] * 120;
    typeCount[type]++;
    
    return {
      ...node,
      position: {
        x: groupPos.x + offset,
        y: groupPos.y + (Math.floor(offset / 240) * 80)
      }
    };
  });
}

module.exports = router; 