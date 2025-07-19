import React, { useState } from 'react';
import { FileUpload } from '@/components/FileUpload';
import { RepositoryLink } from '@/components/RepositoryLink';
import { Button } from '@/components/ui/button';
import { ArrowRight } from 'lucide-react';

const Upload = () => {
  const [uploadedFiles, setUploadedFiles] = useState<File[]>([]);
  const [repositories, setRepositories] = useState<string[]>([]);

  const handleFilesSelected = (files: File[]) => {
    setUploadedFiles(prev => [...prev, ...files]);
  };

  const handleRepositoryAdded = (url: string) => {
    setRepositories(prev => [...prev, url]);
  };

  const handleStartAnalysis = () => {
    if (uploadedFiles.length > 0 || repositories.length > 0) {
      // Here you would typically navigate to analysis page or trigger analysis
      console.log('Starting analysis with:', { uploadedFiles, repositories });
    }
  };

  return (
    <div className="min-h-screen bg-background">
      {/* Upload Section */}
      <section className="py-20 px-4 sm:px-6 lg:px-8">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-16 space-y-4">
            <h2 className="text-3xl md:text-4xl font-bold text-foreground">
              Upload Your COBOL Assets
            </h2>
            <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
              Choose how you'd like to provide your COBOL code for analysis. 
              Upload files directly or connect your repository for comprehensive analysis.
            </p>
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 mb-12">
            <div className="animate-fade-in">
              <FileUpload onFilesSelected={handleFilesSelected} />
            </div>
            <div className="animate-fade-in">
              <RepositoryLink onRepositoryAdded={handleRepositoryAdded} />
            </div>
          </div>

          {/* Action Button */}
          {(uploadedFiles.length > 0 || repositories.length > 0) && (
            <div className="text-center animate-scale-in">
              <div className="p-6 rounded-lg bg-gradient-card backdrop-blur-sm border border-border max-w-md mx-auto">
                <h3 className="text-lg font-semibold mb-2">Ready to Start?</h3>
                <p className="text-muted-foreground text-sm mb-4">
                  {uploadedFiles.length > 0 && `${uploadedFiles.length} file(s) uploaded`}
                  {uploadedFiles.length > 0 && repositories.length > 0 && ' • '}
                  {repositories.length > 0 && `${repositories.length} repository(s) connected`}
                </p>
                <Button 
                  variant="hero" 
                  size="lg" 
                  onClick={handleStartAnalysis}
                  className="group"
                >
                  Start Modernization Analysis
                  <ArrowRight className="w-4 h-4 transition-transform group-hover:translate-x-1" />
                </Button>
              </div>
            </div>
          )}
        </div>
      </section>
    </div>
  );
};

export default Upload;