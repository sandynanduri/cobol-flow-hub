
import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { FileUpload } from '@/components/FileUpload';
import { RepositoryLink } from '@/components/RepositoryLink';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ArrowRight, Upload as UploadIcon, GitBranch } from 'lucide-react';
import { NavigationHeader } from '@/components/NavigationHeader';
import { BreadcrumbNavigation } from '@/components/BreadcrumbNavigation';
import { ProgressIndicator } from '@/components/ProgressIndicator';
import { apiService, SessionManager } from '@/services/api';
import { useToast } from '@/hooks/use-toast';

const Upload = () => {
  const navigate = useNavigate();
  const { toast } = useToast();
  const [uploadedFiles, setUploadedFiles] = useState<File[]>([]);
  const [repositories, setRepositories] = useState<string[]>([]);
  const [isAnalyzing, setIsAnalyzing] = useState(false);

  const handleFilesSelected = (files: File[]) => {
    setUploadedFiles(prev => [...prev, ...files]);
  };

  const handleRepositoryAdded = (url: string) => {
    setRepositories(prev => [...prev, url]);
  };

  const handleStartAnalysis = async () => {
    if (uploadedFiles.length === 0 && repositories.length === 0) {
      toast({
        title: "No files uploaded",
        description: "Please upload COBOL files or connect a repository before starting analysis.",
        variant: "destructive"
      });
      return;
    }

    const jobId = SessionManager.getJobId();
    console.log('Starting analysis with jobId:', jobId);
    if (!jobId) {
      toast({
        title: "No active session",
        description: "Please upload files first to start analysis.",
        variant: "destructive"
      });
      return;
    }

    setIsAnalyzing(true);

    try {
      // Start analysis
      console.log('Calling analyzeFiles API with jobId:', jobId);
      const analysisResult = await apiService.analyzeFiles(jobId);
      
      // Cache the analysis results
      SessionManager.cacheAnalysis(analysisResult);
      
      toast({
        title: "Analysis completed",
        description: "COBOL files have been analyzed successfully. Redirecting to dashboard.",
      });

      // Navigate to dashboard page to show analysis results
      navigate('/dashboard');
      
    } catch (error) {
      console.error('Analysis error:', error);
      toast({
        title: "Analysis failed",
        description: error instanceof Error ? error.message : 'Failed to analyze files',
        variant: "destructive"
      });
    } finally {
      setIsAnalyzing(false);
    }
  };

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

  const currentStep = 0; // Stay on upload step until analysis starts

  return (
    <div className="min-h-screen bg-background">
      <NavigationHeader showBackButton={true} />
      
      <div className="container mx-auto px-4 sm:px-6 lg:px-8">
        <BreadcrumbNavigation />
        
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Main Content */}
          <div className="lg:col-span-2 space-y-8">
            <div className="text-center space-y-4">
              <h2 className="text-3xl md:text-4xl font-bold text-foreground">
                Upload Your COBOL Assets
              </h2>
              <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
                Choose how you'd like to provide your COBOL code for analysis. 
                Upload files directly or connect your repository for comprehensive analysis.
              </p>
            </div>

            <Tabs defaultValue="upload" className="w-full">
              <TabsList className="grid w-full grid-cols-2 mb-8">
                <TabsTrigger value="upload" className="flex items-center gap-2">
                  <UploadIcon className="w-4 h-4" />
                  Upload Files
                </TabsTrigger>
                <TabsTrigger value="repository" className="flex items-center gap-2">
                  <GitBranch className="w-4 h-4" />
                  Connect Repository
                </TabsTrigger>
              </TabsList>
              
              <TabsContent value="upload" className="animate-fade-in">
                <FileUpload 
                  onFilesSelected={handleFilesSelected} 
                  maxFileSize={5}
                  showProgress={true}
                />
              </TabsContent>
              
              <TabsContent value="repository" className="animate-fade-in">
                <RepositoryLink onRepositoryAdded={handleRepositoryAdded} />
              </TabsContent>
            </Tabs>

            {/* Action Button */}
            {(uploadedFiles.length > 0 || repositories.length > 0) && (
              <div className="text-center animate-scale-in">
                <Button 
                  variant="hero" 
                  size="lg" 
                  onClick={handleStartAnalysis}
                  disabled={isAnalyzing || (uploadedFiles.length === 0 && repositories.length === 0)}
                  className="group disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {isAnalyzing ? (
                    <>
                      <div className="w-4 h-4 animate-spin rounded-full border-2 border-primary-foreground border-t-transparent mr-2" />
                      Analyzing Files...
                    </>
                  ) : (
                    <>
                      Start Modernization Analysis
                      <ArrowRight className="w-4 h-4 transition-transform group-hover:translate-x-1" />
                    </>
                  )}
                </Button>
              </div>
            )}
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
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Upload;
