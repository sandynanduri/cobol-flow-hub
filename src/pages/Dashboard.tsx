import React from 'react';
import { NavigationHeader } from '@/components/NavigationHeader';
import { BreadcrumbNavigation } from '@/components/BreadcrumbNavigation';
import { ProgressIndicator } from '@/components/ProgressIndicator';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { DependencyGraph } from '@/components/DependencyGraph';
import { 
  FileCode, 
  Settings, 
  FileText, 
  Download, 
  AlertTriangle,
  CheckCircle,
  Code,
  ArrowRight
} from 'lucide-react';

const Dashboard = () => {

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

  // Mock data for the analysis results
  const fileClassification = [
    { category: 'COBOL Programs', count: 1, files: ['BRAKES.CBL'] },
    { category: 'Copybooks', count: 0, files: [] },
    { category: 'Total LOC', count: 103, files: [] },
    { category: 'External CALLs', count: 2, files: ['CALL "TAX01"', 'CALL "UTIL01"'] },
    { category: 'COPY statements', count: 1, files: ['COPY "EMPLOYEE.CPY"'] }
  ];

  const extractedInsights = [
    { type: 'Programs with PERFORM THRU', value: '1' },
    { type: 'Paragraphs Detected', value: '12' },
    { type: 'File I/O Detected', value: 'Yes (OPEN INPUT CUSTOMER)' },
    { type: 'VSAM or SEQUENTIAL', value: 'SEQUENTIAL' },
    { type: 'JCL Mapping', value: 'Not detected (since no JCL uploaded)' }
  ];

  const missingFiles = ['EMPLOYEE.CPY'];

  const actions = [
    {
      icon: FileCode,
      title: 'View Code & Extract Pseudo-code',
      description: 'Go to paragraph-by-paragraph logic breakdown',
      variant: 'default' as const
    },
    {
      icon: Settings,
      title: 'Convert to Java/Python',
      description: 'Run LLM/codegen pipeline to generate output',
      variant: 'secondary' as const
    },
    {
      icon: FileText,
      title: 'Generate Mapping Report',
      description: 'Show table: COBOL → Function → Summary',
      variant: 'outline' as const
    },
    {
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
            {/* Top Header */}
            <div className="text-center space-y-2">
              <h1 className="text-3xl md:text-4xl font-bold text-foreground flex items-center justify-center gap-2">
                <Code className="w-8 h-8 text-primary" />
                Modernization Analysis for Job_20250726_1445
              </h1>
              <p className="text-muted-foreground">
                Analysis complete • Ready for modernization planning
              </p>
            </div>

            {/* Section 1: File Classification Summary */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <FileCode className="w-5 h-5" />
                  File Classification Summary
                </CardTitle>
                <CardDescription>
                  Overview of uploaded files and detected patterns
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Category</TableHead>
                      <TableHead className="text-center">Count</TableHead>
                      <TableHead>File Names</TableHead>
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
                </CardTitle>
                <CardDescription>
                  Visual representation of file dependencies and relationships
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

            {/* Section 3: Extracted Insights */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <CheckCircle className="w-5 h-5 text-green-600" />
                  Extracted Insights
                </CardTitle>
                <CardDescription>
                  Key patterns and structures identified by AI analysis
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Type</TableHead>
                      <TableHead>Value</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {extractedInsights.map((insight, index) => (
                      <TableRow key={index}>
                        <TableCell className="font-medium">{insight.type}</TableCell>
                        <TableCell>
                          <Badge variant="outline">{insight.value}</Badge>
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </CardContent>
            </Card>

            {/* Section 4: Actions */}
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
                <h3 className="text-lg font-semibold mb-4">Analysis Summary</h3>
                <div className="space-y-3">
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-muted-foreground">Files Analyzed</span>
                    <Badge>1</Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-muted-foreground">Lines of Code</span>
                    <Badge>103</Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-muted-foreground">Complexity Score</span>
                    <Badge variant="secondary">Medium</Badge>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-muted-foreground">Est. Effort</span>
                    <Badge variant="outline">2-3 Days</Badge>
                  </div>
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