import React, { useCallback, useState } from 'react';
import { Upload, File, X, CheckCircle } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Card } from '@/components/ui/card';
import { useToast } from '@/hooks/use-toast';

interface FileUploadProps {
  onFilesSelected: (files: File[]) => void;
}

export const FileUpload: React.FC<FileUploadProps> = ({ onFilesSelected }) => {
  const [dragActive, setDragActive] = useState(false);
  const [uploadedFiles, setUploadedFiles] = useState<File[]>([]);
  const { toast } = useToast();

  const handleDrag = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();
    if (e.type === "dragenter" || e.type === "dragover") {
      setDragActive(true);
    } else if (e.type === "dragleave") {
      setDragActive(false);
    }
  }, []);

  const handleDrop = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setDragActive(false);

    const files = Array.from(e.dataTransfer.files);
    const cobolFiles = files.filter(file => 
      /\.(cob|cbl|cobol|cpy|copy|inc)$/i.test(file.name)
    );

    if (cobolFiles.length !== files.length) {
      toast({
        title: "Some files filtered",
        description: "Only COBOL files (.cob, .cbl, .cobol, .cpy, .copy, .inc) are accepted.",
        variant: "destructive"
      });
    }

    if (cobolFiles.length > 0) {
      setUploadedFiles(prev => [...prev, ...cobolFiles]);
      onFilesSelected(cobolFiles);
      toast({
        title: "Files uploaded successfully",
        description: `${cobolFiles.length} COBOL file(s) ready for modernization.`
      });
    }
  }, [onFilesSelected, toast]);

  const handleFileInput = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const files = Array.from(e.target.files || []);
    const cobolFiles = files.filter(file => 
      /\.(cob|cbl|cobol|cpy|copy|inc)$/i.test(file.name)
    );

    if (cobolFiles.length > 0) {
      setUploadedFiles(prev => [...prev, ...cobolFiles]);
      onFilesSelected(cobolFiles);
      toast({
        title: "Files uploaded successfully",
        description: `${cobolFiles.length} COBOL file(s) ready for modernization.`
      });
    }
  }, [onFilesSelected, toast]);

  const removeFile = useCallback((index: number) => {
    setUploadedFiles(prev => prev.filter((_, i) => i !== index));
  }, []);

  return (
    <Card className="p-8 bg-gradient-card backdrop-blur-sm border-2 border-dashed border-border transition-all duration-300 hover:shadow-card">
      <div
        className={`relative transition-all duration-300 ${
          dragActive ? 'scale-105 opacity-80' : ''
        }`}
        onDragEnter={handleDrag}
        onDragLeave={handleDrag}
        onDragOver={handleDrag}
        onDrop={handleDrop}
      >
        <div className="text-center space-y-6">
          <div className={`mx-auto w-16 h-16 rounded-full bg-gradient-primary flex items-center justify-center transition-all duration-300 ${
            dragActive ? 'animate-glow-pulse' : ''
          }`}>
            <Upload className="w-8 h-8 text-primary-foreground" />
          </div>
          
          <div className="space-y-2">
            <h3 className="text-xl font-semibold text-foreground">
              Upload COBOL Files
            </h3>
            <p className="text-muted-foreground">
              Drag and drop your COBOL files here, or click to browse
            </p>
            <p className="text-sm text-muted-foreground">
              Supports: .cob, .cbl, .cobol, .cpy, .copy, .inc
            </p>
          </div>

          <div className="space-y-4">
            <input
              type="file"
              multiple
              accept=".cob,.cbl,.cobol,.cpy,.copy,.inc"
              onChange={handleFileInput}
              className="hidden"
              id="file-upload"
            />
            <label htmlFor="file-upload">
              <Button variant="hero" size="lg" className="cursor-pointer" asChild>
                <span>Choose Files</span>
              </Button>
            </label>
          </div>
        </div>

        {uploadedFiles.length > 0 && (
          <div className="mt-6 space-y-3">
            <h4 className="font-medium flex items-center gap-2">
              <CheckCircle className="w-4 h-4 text-primary" />
              Uploaded Files ({uploadedFiles.length})
            </h4>
            <div className="space-y-2 max-h-32 overflow-y-auto">
              {uploadedFiles.map((file, index) => (
                <div key={index} className="flex items-center justify-between p-2 bg-secondary rounded-md">
                  <div className="flex items-center gap-2">
                    <File className="w-4 h-4 text-primary" />
                    <span className="text-sm font-medium">{file.name}</span>
                    <span className="text-xs text-muted-foreground">
                      ({(file.size / 1024).toFixed(1)} KB)
                    </span>
                  </div>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => removeFile(index)}
                    className="h-6 w-6 p-0"
                  >
                    <X className="w-4 h-4" />
                  </Button>
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </Card>
  );
};