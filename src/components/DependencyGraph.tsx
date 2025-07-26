import React, { useCallback, useState, useMemo } from 'react';
import {
  ReactFlow,
  Node,
  Edge,
  addEdge,
  Connection,
  useNodesState,
  useEdgesState,
  Background,
  Controls,
  MiniMap,
} from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import { Button } from '@/components/ui/button';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';
import { Eye, EyeOff } from 'lucide-react';

const initialNodes: Node[] = [
  {
    id: '1',
    type: 'default',
    position: { x: 250, y: 100 },
    data: { label: 'BRAKES.CBL' },
    style: {
      background: 'hsl(var(--primary))',
      color: 'hsl(var(--primary-foreground))',
      border: '2px solid hsl(var(--primary))',
      borderRadius: '8px',
      fontSize: '12px',
      fontWeight: '500',
    },
  },
  {
    id: '2',
    type: 'default',
    position: { x: 100, y: 200 },
    data: { label: 'EMPLOYEE.CPY' },
    style: {
      background: 'hsl(var(--background))',
      color: 'hsl(var(--foreground))',
      border: '2px dashed hsl(var(--destructive))',
      borderRadius: '8px',
      fontSize: '12px',
      fontWeight: '500',
    },
  },
  {
    id: '3',
    type: 'default',
    position: { x: 400, y: 200 },
    data: { label: 'TAX01' },
    style: {
      background: 'hsl(var(--background))',
      color: 'hsl(var(--foreground))',
      border: '2px dashed hsl(var(--destructive))',
      borderRadius: '8px',
      fontSize: '12px',
      fontWeight: '500',
    },
  },
  {
    id: '4',
    type: 'default',
    position: { x: 500, y: 100 },
    data: { label: 'UTIL01' },
    style: {
      background: 'hsl(var(--secondary))',
      color: 'hsl(var(--secondary-foreground))',
      border: '2px solid hsl(var(--secondary))',
      borderRadius: '4px',
      fontSize: '12px',
      fontWeight: '500',
      transform: 'rotate(45deg)',
      width: '80px',
      height: '80px',
    },
  },
];

const initialEdges: Edge[] = [
  {
    id: 'e1-2',
    source: '1',
    target: '2',
    type: 'straight',
    style: { stroke: 'hsl(var(--primary))', strokeWidth: 2 },
    label: 'COPY',
    labelStyle: { fontSize: '10px', fontWeight: '500' },
  },
  {
    id: 'e1-3',
    source: '1',
    target: '3',
    type: 'straight',
    style: { stroke: 'hsl(var(--destructive))', strokeWidth: 2, strokeDasharray: '8,4' },
    label: 'CALL (Missing)',
    labelStyle: { fontSize: '10px', fontWeight: '500', fill: 'hsl(var(--destructive))' },
  },
  {
    id: 'e1-4',
    source: '1',
    target: '4',
    type: 'straight',
    style: { stroke: 'hsl(var(--primary))', strokeWidth: 2, strokeDasharray: '5,5' },
    label: 'CALL',
    labelStyle: { fontSize: '10px', fontWeight: '500' },
  },
];

export const DependencyGraph: React.FC = () => {
  const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
  const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);
  const [showMiniMap, setShowMiniMap] = useState(true);

  const onConnect = useCallback(
    (params: Connection) => setEdges((eds) => addEdge(params, eds)),
    [setEdges]
  );

  // Calculate minimap size based on node count
  const minimapSize = useMemo(() => {
    const nodeCount = nodes.length;
    if (nodeCount <= 3) {
      return { width: 120, height: 80 };
    } else if (nodeCount <= 6) {
      return { width: 150, height: 100 };
    }
    return { width: 200, height: 120 };
  }, [nodes.length]);

  return (
    <div className="w-full h-full relative">
      {/* Minimap Toggle */}
      <div className="absolute top-4 right-4 z-10">
        <TooltipProvider>
          <Tooltip>
            <TooltipTrigger asChild>
              <Button
                variant="outline"
                size="sm"
                onClick={() => setShowMiniMap(!showMiniMap)}
                className="bg-background/80 backdrop-blur-sm border-border hover:bg-accent"
              >
                {showMiniMap ? <EyeOff className="h-4 w-4" /> : <Eye className="h-4 w-4" />}
              </Button>
            </TooltipTrigger>
            <TooltipContent>
              <p>{showMiniMap ? 'Hide' : 'Show'} minimap - Navigate large dependency graphs</p>
            </TooltipContent>
          </Tooltip>
        </TooltipProvider>
      </div>

      <ReactFlow
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onConnect={onConnect}
        fitView
        attributionPosition="bottom-left"
        nodesDraggable={false}
        nodesConnectable={false}
        elementsSelectable={false}
        panOnDrag={false}
        zoomOnScroll={false}
        style={{ background: 'transparent' }}
      >
        <Background gap={20} size={1} color="hsl(var(--muted-foreground))" />
        
        {showMiniMap && (
          <TooltipProvider>
            <Tooltip>
              <TooltipTrigger asChild>
                <div className="react-flow__minimap-wrapper">
                  <MiniMap
                    nodeColor="hsl(var(--primary))"
                    nodeStrokeColor="hsl(var(--primary-foreground))"
                    nodeStrokeWidth={2}
                    maskColor="hsl(var(--muted) / 0.3)"
                    pannable
                    zoomable
                    style={{
                      background: 'hsl(var(--card))',
                      border: '1px solid hsl(var(--border))',
                      borderRadius: '6px',
                      boxShadow: '0 4px 6px -1px hsl(var(--muted-foreground) / 0.1)',
                      width: `${minimapSize.width}px`,
                      height: `${minimapSize.height}px`,
                    }}
                  />
                </div>
              </TooltipTrigger>
              <TooltipContent side="left">
                <p>Minimap - Click and drag to navigate the dependency graph</p>
              </TooltipContent>
            </Tooltip>
          </TooltipProvider>
        )}
        
        <Controls 
          style={{
            background: 'hsl(var(--card))',
            border: '1px solid hsl(var(--border))',
            borderRadius: '6px',
            boxShadow: '0 4px 6px -1px hsl(var(--muted-foreground) / 0.1)',
          }}
        />
      </ReactFlow>
    </div>
  );
};