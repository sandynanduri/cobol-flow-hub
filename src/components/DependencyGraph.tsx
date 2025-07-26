import React, { useCallback, useState, useMemo, useEffect } from 'react';
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
import { Eye, EyeOff, Loader, AlertCircle } from 'lucide-react';
import { apiService, SessionManager, GraphData } from '@/services/api';

export const DependencyGraph: React.FC = () => {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const [showMiniMap, setShowMiniMap] = useState(true);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const onConnect = useCallback(
    (params: Connection) => setEdges((eds) => addEdge(params, eds)),
    [setEdges]
  );

  // Load graph data from API
  useEffect(() => {
    const loadGraphData = async () => {
      try {
        const jobId = SessionManager.getJobId();
        if (!jobId) {
          setError('No analysis session found');
          setLoading(false);
          return;
        }

        const graphData = await apiService.getGraphData(jobId);
        setNodes(graphData.nodes);
        setEdges(graphData.edges);
        setError(null);
      } catch (err) {
        console.error('Failed to load graph data:', err);
        setError(err instanceof Error ? err.message : 'Failed to load dependency graph');
      } finally {
        setLoading(false);
      }
    };

    loadGraphData();
  }, [setNodes, setEdges]);

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

  // Loading state
  if (loading) {
    return (
      <div className="w-full h-full flex items-center justify-center">
        <div className="text-center space-y-2">
          <Loader className="w-6 h-6 animate-spin mx-auto text-primary" />
          <p className="text-sm text-muted-foreground">Loading dependency graph...</p>
        </div>
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="w-full h-full flex items-center justify-center">
        <div className="text-center space-y-2">
          <AlertCircle className="w-6 h-6 mx-auto text-destructive" />
          <p className="text-sm text-muted-foreground">{error}</p>
        </div>
      </div>
    );
  }

  // Empty state
  if (nodes.length === 0) {
    return (
      <div className="w-full h-full flex items-center justify-center">
        <div className="text-center space-y-2">
          <p className="text-sm text-muted-foreground">No dependencies found</p>
        </div>
      </div>
    );
  }

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