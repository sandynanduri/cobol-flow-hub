import React, { useCallback } from 'react';
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
    style: { stroke: 'hsl(var(--primary))', strokeWidth: 2, strokeDasharray: '5,5' },
    label: 'CALL',
    labelStyle: { fontSize: '10px', fontWeight: '500' },
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

  const onConnect = useCallback(
    (params: Connection) => setEdges((eds) => addEdge(params, eds)),
    [setEdges]
  );

  return (
    <div className="w-full h-full">
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
        <MiniMap
          nodeColor="hsl(var(--primary))"
          nodeStrokeWidth={3}
          pannable
          zoomable
          style={{
            background: 'hsl(var(--background))',
            border: '1px solid hsl(var(--border))',
          }}
        />
        <Controls 
          style={{
            background: 'hsl(var(--background))',
            border: '1px solid hsl(var(--border))',
          }}
        />
      </ReactFlow>
    </div>
  );
};