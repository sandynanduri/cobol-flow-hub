import React, { useState, useEffect } from 'react';
import { NavigationHeader } from '@/components/NavigationHeader';
import { BreadcrumbNavigation } from '@/components/BreadcrumbNavigation';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ScrollArea } from '@/components/ui/scroll-area';
import { 
  FileCode, 
  Download, 
  Copy,
  ArrowLeft,
  Eye,
  FileText,
  Code,
  Loader
} from 'lucide-react';
import { apiService, SessionManager, AnalysisResponse } from '@/services/api';
import { useToast } from '@/hooks/use-toast';
import { useSearchParams, useNavigate } from 'react-router-dom';

const FilePreview = () => {
  const { toast } = useToast();
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const fileName = searchParams.get('file');
  
  const [analysis, setAnalysis] = useState<AnalysisResponse | null>(null);
  const [fileContent, setFileContent] = useState<string>('');
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [activeTab, setActiveTab] = useState('preview');

  useEffect(() => {
    const loadFileData = async () => {
      try {
        // Get cached analysis
        let analysisData = SessionManager.getCachedAnalysis();
        
        if (!analysisData) {
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
        
        // Find the specific file data
        const selectedFile = analysisData.files?.find(file => file.fileName === fileName);
        if (!selectedFile) {
          setError(`File "${fileName}" not found in analysis.`);
          setLoading(false);
          return;
        }

        // For now, we'll simulate file content loading
        // In a real implementation, you'd fetch the actual file content
        setFileContent(`// COBOL File: ${fileName}
// This is a preview of the file content
// In a real implementation, this would load the actual file content

IDENTIFICATION DIVISION.
PROGRAM-ID. ${fileName.replace(/\.[^/.]+$/, "").toUpperCase()}.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

DATA DIVISION.
FILE SECTION.

WORKING-STORAGE SECTION.

PROCEDURE DIVISION.
MAIN-PARAGRAPH.
    DISPLAY "Hello from ${fileName}".
    STOP RUN.`);
        
        setError(null);
      } catch (err) {
        console.error('Failed to load file data:', err);
        setError(err instanceof Error ? err.message : 'Failed to load file data');
        toast({
          title: "Failed to load file",
          description: "Could not retrieve file data. Please try again.",
          variant: "destructive"
        });
      } finally {
        setLoading(false);
      }
    };

    if (fileName) {
      loadFileData();
    } else {
      setError('No file specified.');
      setLoading(false);
    }
  }, [fileName, toast]);

  const handleCopyContent = () => {
    navigator.clipboard.writeText(fileContent);
    toast({
      title: "Content copied",
      description: "File content has been copied to clipboard.",
    });
  };

  const handleDownload = () => {
    const blob = new Blob([fileContent], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = fileName || 'file.cbl';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    
    toast({
      title: "File downloaded",
      description: `${fileName} has been downloaded.`,
    });
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-background">
        <NavigationHeader showBackButton={true} />
        <div className="container mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-center min-h-[400px]">
            <div className="text-center space-y-4">
              <Loader className="w-8 h-8 animate-spin mx-auto text-primary" />
              <h2 className="text-xl font-semibold">Loading File Preview...</h2>
              <p className="text-muted-foreground">Please wait while we load the file content.</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-background">
        <NavigationHeader showBackButton={true} />
        <div className="container mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-center min-h-[400px]">
            <div className="text-center space-y-4">
              <FileText className="w-8 h-8 mx-auto text-destructive" />
              <h2 className="text-xl font-semibold">File Not Found</h2>
              <p className="text-muted-foreground">{error}</p>
              <Button onClick={() => navigate('/dashboard')}>
                Return to Dashboard
              </Button>
            </div>
          </div>
        </div>
      </div>
    );
  }

  const selectedFile = analysis?.files?.find(file => file.fileName === fileName);

  return (
    <div className="min-h-screen bg-background">
      <NavigationHeader showBackButton={true} />
      
      <div className="container mx-auto px-4 sm:px-6 lg:px-8">
        <BreadcrumbNavigation />
        
        <div className="space-y-6">
          {/* Header */}
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-4">
              <Button 
                variant="outline" 
                onClick={() => navigate('/dashboard')}
                className="flex items-center gap-2"
              >
                <ArrowLeft className="w-4 h-4" />
                Back to Dashboard
              </Button>
              
              <div>
                <h1 className="text-2xl font-bold flex items-center gap-2">
                  <FileCode className="w-6 h-6 text-primary" />
                  {fileName}
                </h1>
                {selectedFile && (
                  <div className="flex items-center gap-2 mt-1">
                    <Badge variant="outline">{selectedFile.type}</Badge>
                    <Badge variant="secondary">{selectedFile.loc} LOC</Badge>
                  </div>
                )}
              </div>
            </div>
            
            <div className="flex items-center gap-2">
              <Button variant="outline" onClick={handleCopyContent}>
                <Copy className="w-4 h-4 mr-2" />
                Copy
              </Button>
              <Button variant="outline" onClick={handleDownload}>
                <Download className="w-4 h-4 mr-2" />
                Download
              </Button>
            </div>
          </div>

          {/* File Info Card */}
          {selectedFile && (
            <Card>
              <CardHeader>
                <CardTitle>File Information</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">File Type</p>
                    <p className="text-lg font-semibold">{selectedFile.type}</p>
                  </div>
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">Lines of Code</p>
                    <p className="text-lg font-semibold">{selectedFile.loc}</p>
                  </div>
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">Dependencies</p>
                    <p className="text-lg font-semibold">
                      {(selectedFile.dependencies?.copy?.length || 0) + (selectedFile.dependencies?.call?.length || 0)}
                    </p>
                  </div>
                </div>
                
                {/* Dependencies */}
                {(selectedFile.dependencies?.copy?.length || selectedFile.dependencies?.call?.length) && (
                  <div className="mt-4 space-y-2">
                    {selectedFile.dependencies?.copy?.length > 0 && (
                      <div>
                        <p className="text-sm font-medium text-muted-foreground">COPY Dependencies</p>
                        <div className="flex flex-wrap gap-1 mt-1">
                          {selectedFile.dependencies.copy.map((dep, index) => (
                            <Badge key={index} variant="outline" className="text-xs">
                              {dep}
                            </Badge>
                          ))}
                        </div>
                      </div>
                    )}
                    
                    {selectedFile.dependencies?.call?.length > 0 && (
                      <div>
                        <p className="text-sm font-medium text-muted-foreground">CALL Dependencies</p>
                        <div className="flex flex-wrap gap-1 mt-1">
                          {selectedFile.dependencies.call.map((dep, index) => (
                            <Badge key={index} variant="outline" className="text-xs">
                              {dep}
                            </Badge>
                          ))}
                        </div>
                      </div>
                    )}
                  </div>
                )}
              </CardContent>
            </Card>
          )}

          {/* File Content Tabs */}
          <Card>
            <CardHeader>
              <CardTitle>File Content</CardTitle>
            </CardHeader>
            <CardContent>
              <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
                <TabsList className="grid w-full grid-cols-3">
                  <TabsTrigger value="preview" className="flex items-center gap-2">
                    <Eye className="w-4 h-4" />
                    Preview
                  </TabsTrigger>
                  <TabsTrigger value="raw" className="flex items-center gap-2">
                    <Code className="w-4 h-4" />
                    Raw Code
                  </TabsTrigger>
                  <TabsTrigger value="analysis" className="flex items-center gap-2">
                    <FileText className="w-4 h-4" />
                    Analysis
                  </TabsTrigger>
                </TabsList>
                
                <TabsContent value="preview" className="mt-4">
                  <ScrollArea className="h-[600px] w-full rounded-md border p-4">
                    <pre className="text-sm font-mono whitespace-pre-wrap">
                      {fileContent}
                    </pre>
                  </ScrollArea>
                </TabsContent>
                
                <TabsContent value="raw" className="mt-4">
                  <ScrollArea className="h-[600px] w-full rounded-md border p-4 bg-muted/50">
                    <pre className="text-sm font-mono whitespace-pre-wrap">
                      {fileContent}
                    </pre>
                  </ScrollArea>
                </TabsContent>
                
                <TabsContent value="analysis" className="mt-4">
                  <div className="space-y-4">
                    <div className="text-center py-8 text-muted-foreground">
                      <FileText className="w-8 h-8 mx-auto mb-2" />
                      <p>Detailed code analysis coming soon...</p>
                      <p className="text-sm mt-1">This will include complexity metrics, potential issues, and modernization suggestions.</p>
                    </div>
                  </div>
                </TabsContent>
              </Tabs>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
};

export default FilePreview;