import React, { useState, useEffect, useMemo } from 'react';
import { NavigationHeader } from '@/components/NavigationHeader';
import { BreadcrumbNavigation } from '@/components/BreadcrumbNavigation';
import { ProgressIndicator } from '@/components/ProgressIndicator';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { DependencyGraph } from '@/components/DependencyGraph';
import { 
  FileCode, 
  Settings, 
  FileText, 
  Download, 
  AlertTriangle,
  CheckCircle,
  Code,
  ArrowRight,
  Loader,
  ChevronDown,
  Eye
} from 'lucide-react';
import { apiService, SessionManager, AnalysisResponse } from '@/services/api';
import { useToast } from '@/hooks/use-toast';
import { useNavigate } from 'react-router-dom';

const Dashboard = () => {
  const { toast } = useToast();
  const navigate = useNavigate();
  const [analysis, setAnalysis] = useState<AnalysisResponse | null>(null);
  const [selectedProgram, setSelectedProgram] = useState<string>('');
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const progressSteps = [
    {
      id: 'upload',
      label: 'Upload Assets',
      description: 'Upload COBOL files or connect repository'
    },
    {
      id: 'analyze',
      label: 'Analyze Code',
      description: 'AI analyzes your COBOL codebase'
    },
    {
      id: 'modernize',
      label: 'Modernization Plan',
      description: 'Generate modernization recommendations'
    },
    {
      id: 'implement',
      label: 'Implementation',
      description: 'Execute the modernization strategy'
    }
  ];

  const currentStep = 1; // Analyze step

  // Load analysis data on component mount
  useEffect(() => {
    const loadAnalysis = async () => {
      try {
        // First try to get cached analysis
        let analysisData = SessionManager.getCachedAnalysis();
        
        if (!analysisData) {
          // If no cache, get from backend
          const jobId = SessionManager.getJobId();
          if (!jobId) {
            setError('No analysis session found. Please upload files first.');
            setLoading(false);
            return;
          }
          
          analysisData = await apiService.getAnalysis(jobId);
          SessionManager.cacheAnalysis(analysisData);
        }
        
        setAnalysis(analysisData);
        
        // Set default selected program to the first COBOL program
        const programs = getCobolPrograms(analysisData);
        if (programs.length > 0 && !selectedProgram) {
          setSelectedProgram(programs[0].fileName);
        }
        
        setError(null);
      } catch (err) {
        console.error('Failed to load analysis:', err);
        setError(err instanceof Error ? err.message : 'Failed to load analysis data');
        toast({
          title: "Failed to load analysis",
          description: "Could not retrieve analysis data. Please try running analysis again.",
          variant: "destructive"
        });
      } finally {
        setLoading(false);
      }
    };

    loadAnalysis();
  }, [toast, selectedProgram]);

  // Show loading state
  if (loading) {
    return (
      <div className="min-h-screen bg-background">
        <NavigationHeader showBackButton={true} />
        <div className="container mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-center min-h-[400px]">
            <div className="text-center space-y-4">
              <Loader className="w-8 h-8 animate-spin mx-auto text-primary" />
              <h2 className="text-xl font-semibold">Loading Analysis Results...</h2>
              <p className="text-muted-foreground">Please wait while we retrieve your COBOL analysis data.</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  // Show error state
  if (error || !analysis) {
    return (
      <div className="min-h-screen bg-background">
        <NavigationHeader showBackButton={true} />
        <div className="container mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-center min-h-[400px]">
            <div className="text-center space-y-4">
              <AlertTriangle className="w-8 h-8 mx-auto text-destructive" />
              <h2 className="text-xl font-semibold">Analysis Not Found</h2>
              <p className="text-muted-foreground">{error || 'No analysis data available.'}</p>
              <Button onClick={() => window.location.href = '/upload'}>
                Return to Upload
              </Button>
            </div>
          </div>
        </div>
      </div>
    );
  }

  // Helper function to get COBOL programs from analysis
  const getCobolPrograms = (analysisData: AnalysisResponse) => {
    return (analysisData?.files || []).filter(file => 
      file.type === 'program' && 
      (file.fileName.endsWith('.cbl') || file.fileName.endsWith('.cobol') || file.fileName.endsWith('.cob'))
    );
  };

  // Get filtered data based on selected program
  const filteredData = useMemo(() => {
    if (!analysis || !selectedProgram) {
      return {
        fileClassification: analysis?.fileClassification || [],
        missingFiles: analysis?.missingFiles || [],
        programData: null,
        totalLOC: analysis?.summary?.totalLOC || 0
      };
    }

    const selectedFile = analysis.files?.find(file => file.fileName === selectedProgram);
    if (!selectedFile) {
      return {
        fileClassification: analysis.fileClassification || [],
        missingFiles: analysis.missingFiles || [],
        programData: null,
        totalLOC: analysis.summary?.totalLOC || 0
      };
    }

    // Create program-specific file classification
    const programSpecificClassification = [
      {
        category: 'Selected Program',
        count: 1,
        files: [selectedFile.fileName]
      },
      {
        category: 'Lines of Code',
        count: selectedFile.loc || 0,
        files: []
      },
      {
        category: 'COPY Dependencies',
        count: selectedFile.dependencies?.copy?.length || 0,
        files: selectedFile.dependencies?.copy || []
      },
      {
        category: 'CALL Dependencies',
        count: selectedFile.dependencies?.call?.length || 0,
        files: selectedFile.dependencies?.call || []
      }
    ];

    // Filter missing files to only those referenced by selected program
    const programMissingFiles = [
      ...(selectedFile.dependencies?.copy || []),
      ...(selectedFile.dependencies?.call || [])
    ].filter(dep => 
      (analysis.missingFiles || []).includes(dep)
    );

    return {
      fileClassification: programSpecificClassification,
      missingFiles: programMissingFiles,
      programData: selectedFile,
      totalLOC: selectedFile.loc || 0
    };
  }, [analysis, selectedProgram]);

  // Extract data from analysis response
  const fileClassification = filteredData.fileClassification;
  const missingFiles = filteredData.missingFiles;
  const programs = analysis ? getCobolPrograms(analysis) : [];

  const handleAction = async (actionType: string) => {
    const jobId = SessionManager.getJobId();
    if (!jobId) {
      toast({
        title: "No active session",
        description: "Please upload files first.",
        variant: "destructive"
      });
      return;
    }

    try {
      switch (actionType) {
        case 'view-code':
          // Navigate to detailed file analysis (could be a new page)
          toast({
            title: "Feature coming soon",
            description: "Detailed code analysis view is being developed.",
          });
          break;

        case 'convert':
          const conversionResult = await apiService.convertToLanguage(jobId, 'java');
          toast({
            title: "Conversion plan generated",
            description: "Java conversion plan has been created.",
          });
          console.log('Conversion result:', conversionResult);
          break;

        case 'mapping':
          const mappingBlob = await apiService.getMappingTable(jobId, 'java');
          apiService.downloadFile(mappingBlob, `cobol-java-mapping-${jobId}.json`);
          toast({
            title: "Mapping table downloaded",
            description: "COBOL to Java mapping table has been exported.",
          });
          break;

        case 'export':
          const metadataBlob = await apiService.exportMetadata(jobId);
          apiService.downloadFile(metadataBlob, `cobol-analysis-${jobId}.json`);
          toast({
            title: "Analysis exported",
            description: "Complete analysis metadata has been downloaded.",
          });
          break;

        default:
          toast({
            title: "Action not implemented",
            description: "This feature is being developed.",
          });
      }
    } catch (error) {
      console.error('Action failed:', error);
      toast({
        title: "Action failed",
        description: error instanceof Error ? error.message : "An error occurred",
        variant: "destructive"
      });
    }
  };

  const actions = [
    {
      id: 'view-code',
      icon: FileCode,
      title: 'View Code & Extract Pseudo-code',
      description: 'Go to paragraph-by-paragraph logic breakdown',
      variant: 'default' as const
    },
    {
      id: 'convert',
      icon: Settings,
      title: 'Convert to Java/Python',
      description: 'Run LLM/codegen pipeline to generate output',
      variant: 'secondary' as const
    },
    {
      id: 'mapping',
      icon: FileText,
      title: 'Generate Mapping Report',
      description: 'Show table: COBOL → Function → Summary',
      variant: 'outline' as const
    },
    {
      id: 'export',
      icon: Download,
      title: 'Export JSON Summary',
      description: 'Download metadata.json for audit',
      variant: 'outline' as const
    }
  ];

  return (
    <div className="min-h-screen bg-background">
      <NavigationHeader showBackButton={true} />
      
      <div className="container mx-auto px-4 sm:px-6 lg:px-8">
        <BreadcrumbNavigation />
        
        <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
          {/* Main Content */}
          <div className="lg:col-span-3 space-y-8">
            {/* Top Header with Program Selector */}
            <div className="text-center space-y-4">
              <h1 className="text-3xl md:text-4xl font-bold text-foreground flex items-center justify-center gap-2">
                <Code className="w-8 h-8 text-primary" />
                Modernization Analysis Dashboard
              </h1>
              
              {/* Program Selector */}
              {programs.length > 1 && (
                <div className="flex items-center justify-center gap-3">
                  <span className="text-sm font-medium text-muted-foreground">Select Main Program:</span>
                  <Select value={selectedProgram} onValueChange={setSelectedProgram}>
                    <SelectTrigger className="w-[300px]">
                      <SelectValue placeholder="Choose a COBOL program" />
                    </SelectTrigger>
                    <SelectContent>
                      {programs.map((program) => (
                        <SelectItem key={program.fileName} value={program.fileName}>
                          <div className="flex items-center gap-2">
                            <FileCode className="w-4 h-4" />
                            <span>{program.fileName}</span>
                            <Badge variant="outline" className="text-xs">
                              {program.loc} LOC
                            </Badge>
                          </div>
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>
              )}
              
              <p className="text-muted-foreground">
                {programs.length > 1 
                  ? `Analyzing ${selectedProgram || 'selected program'} • ${programs.length} programs total`
                  : 'Analysis complete • Ready for modernization planning'
                }
              </p>
            </div>

            {/* Section 1: Program Analysis Summary */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <FileCode className="w-5 h-5" />
                  {programs.length > 1 ? 'Program-Specific Analysis' : 'File Classification Summary'}
                </CardTitle>
                <CardDescription>
                  {programs.length > 1 
                    ? `Detailed analysis for ${selectedProgram || 'selected program'}`
                    : 'Overview of uploaded files and detected patterns'
                  }
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Category</TableHead>
                      <TableHead className="text-center">Count</TableHead>
                      <TableHead>File Names</TableHead>
                      <TableHead className="text-center">Actions</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {fileClassification.map((row, index) => (
                      <TableRow key={index}>
                        <TableCell className="font-medium">{row.category}</TableCell>
                        <TableCell className="text-center">
                          <Badge variant="secondary">{row.count}</Badge>
                        </TableCell>
                        <TableCell>
                          {row.files.length > 0 ? (
                            <div className="flex flex-wrap gap-1">
                              {row.files.map((file, fileIndex) => (
                                <Badge key={fileIndex} variant="outline" className="text-xs">
                                  {file}
                                </Badge>
                              ))}
                            </div>
                          ) : (
                            <span className="text-muted-foreground">—</span>
                          )}
                        </TableCell>
                        <TableCell className="text-center">
                          {row.files.length > 0 && row.category === 'Selected Program' ? (
                            <Button 
                              variant="outline" 
                              size="sm"
                              onClick={() => navigate(`/file-preview?file=${encodeURIComponent(row.files[0])}`)}
                              className="flex items-center gap-1"
                            >
                              <Eye className="w-3 h-3" />
                              Preview
                            </Button>
                          ) : row.files.length > 0 && (row.category.includes('COPY') || row.category.includes('CALL')) ? (
                            <div className="flex flex-wrap gap-1">
                              {row.files.map((file, fileIndex) => (
                                <Button 
                                  key={fileIndex}
                                  variant="ghost" 
                                  size="sm"
                                  onClick={() => navigate(`/file-preview?file=${encodeURIComponent(file)}`)}
                                  className="h-6 px-2 text-xs"
                                >
                                  <Eye className="w-3 h-3" />
                                </Button>
                              ))}
                            </div>
                          ) : (
                            <span className="text-muted-foreground">—</span>
                          )}
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>


                {/* Missing Files Alert */}
                {missingFiles.length > 0 && (
                  <div className="mt-4 p-3 border border-yellow-200 bg-yellow-50 rounded-md">
                    <div className="flex items-center gap-2 text-yellow-800">
                      <AlertTriangle className="w-4 h-4" />
                      <span className="font-medium">Missing Referenced Files:</span>
                    </div>
                    <div className="mt-1 flex flex-wrap gap-1">
                      {missingFiles.map((file, index) => (
                        <Badge key={index} variant="destructive" className="text-xs">
                          {file}
                        </Badge>
                      ))}
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>

            {/* Section 2: Dependency Graph */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <div className="w-5 h-5 border-2 border-primary rounded"></div>
                  Dependency Graph
                  {programs.length > 1 && selectedProgram && (
                    <Badge variant="outline" className="ml-2">
                      {selectedProgram}
                    </Badge>
                  )}
                </CardTitle>
                <CardDescription>
                  {programs.length > 1 
                    ? `Dependencies and relationships for ${selectedProgram || 'selected program'}`
                    : 'Visual representation of file dependencies and relationships'
                  }
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="h-64 border rounded-md bg-muted/30">
                  <DependencyGraph />
                </div>
                <div className="mt-4 flex justify-center gap-6 text-sm text-muted-foreground">
                  <div className="flex items-center gap-2">
                    <div className="w-3 h-3 border border-current rounded"></div>
                    <span>COBOL Programs</span>
                  </div>
                  <div className="flex items-center gap-2">
                    <div className="w-3 h-3 border-2 border-red-500 rounded"></div>
                    <span>Missing Files</span>
                  </div>
                  <div className="flex items-center gap-2">
                    <div className="w-4 h-0 border-t border-current"></div>
                    <span>COPY</span>
                  </div>
                  <div className="flex items-center gap-2">
                    <div className="w-4 h-0 border-t border-dashed border-current"></div>
                    <span>CALL</span>
                  </div>
                </div>
              </CardContent>
            </Card>

            {/* Section 3: Actions */}
            <Card>
              <CardHeader>
                <CardTitle>Next Steps</CardTitle>
                <CardDescription>
                  Choose your next action to continue the modernization process
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  {actions.map((action, index) => (
                    <Button
                      key={index}
                      variant={action.variant}
                      className="h-auto p-4 flex flex-col items-start gap-2 text-left"
                      onClick={() => handleAction(action.id)}
                    >
                      <div className="flex items-center gap-2 w-full">
                        <action.icon className="w-5 h-5" />
                        <span className="font-medium">{action.title}</span>
                        <ArrowRight className="w-4 h-4 ml-auto" />
                      </div>
                      <p className="text-xs text-muted-foreground">
                        {action.description}
                      </p>
                    </Button>
                  ))}
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Progress Sidebar */}
          <div className="lg:col-span-1">
            <div className="sticky top-20 space-y-6">
              <div className="p-6 rounded-lg border bg-card">
                <h3 className="text-lg font-semibold mb-4">Modernization Process</h3>
                <ProgressIndicator 
                  steps={progressSteps} 
                  currentStep={currentStep}
                />
              </div>

              {/* Quick Stats */}
              <div className="p-6 rounded-lg border bg-card">
                <h3 className="text-lg font-semibold mb-4">
                  {programs.length > 1 ? 'Program Summary' : 'Analysis Summary'}
                </h3>
                <div className="space-y-3">
                  {programs.length > 1 ? (
                    <>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Total Programs</span>
                        <Badge>{programs.length}</Badge>
                      </div>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Selected Program LOC</span>
                        <Badge>{filteredData.totalLOC}</Badge>
                      </div>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Dependencies</span>
                        <Badge>{
                          (filteredData.programData?.dependencies?.copy?.length || 0) +
                          (filteredData.programData?.dependencies?.call?.length || 0)
                        }</Badge>
                      </div>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Missing Files</span>
                        <Badge variant={missingFiles.length > 0 ? 'destructive' : 'default'}>
                          {missingFiles.length}
                        </Badge>
                      </div>
                    </>
                  ) : (
                    <>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Files Analyzed</span>
                        <Badge>{analysis.files?.length || 0}</Badge>
                      </div>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Lines of Code</span>
                        <Badge>{analysis.summary?.totalLOC || 0}</Badge>
                      </div>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Complexity Score</span>
                        <Badge variant={
                          analysis.complexity === 'HIGH' ? 'destructive' : 
                          analysis.complexity === 'MEDIUM' ? 'secondary' : 'default'
                        }>
                          {analysis.complexity || 'Unknown'}
                        </Badge>
                      </div>
                      <div className="flex justify-between items-center">
                        <span className="text-sm text-muted-foreground">Est. Effort</span>
                        <Badge variant="outline">{analysis.estimatedEffort || 'TBD'}</Badge>
                      </div>
                    </>
                  )}
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Dashboard;