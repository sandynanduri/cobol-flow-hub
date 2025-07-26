import React, { useCallback, useState } from 'react';
import { Upload, File, X, CheckCircle, AlertCircle, FileText, Trash2 } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Card } from '@/components/ui/card';
import { Progress } from '@/components/ui/progress';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { useToast } from '@/hooks/use-toast';
import { apiService, SessionManager } from '@/services/api';

interface FileUploadProps {
  onFilesSelected: (files: File[]) => void;
  maxFileSize?: number; // in MB, default 5MB
  showProgress?: boolean;
}

interface FileWithError {
  file: File;
  error?: string;
}

export const FileUpload: React.FC<FileUploadProps> = ({ 
  onFilesSelected, 
  maxFileSize = 5,
  showProgress = false 
}) => {
  const [dragActive, setDragActive] = useState(false);
  const [uploadedFiles, setUploadedFiles] = useState<File[]>([]);
  const [rejectedFiles, setRejectedFiles] = useState<FileWithError[]>([]);
  const [uploadProgress, setUploadProgress] = useState(0);
  const [isUploading, setIsUploading] = useState(false);
  const { toast } = useToast();

  const maxFileSizeBytes = maxFileSize * 1024 * 1024;

  const handleDrag = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();
    if (e.type === "dragenter" || e.type === "dragover") {
      setDragActive(true);
    } else if (e.type === "dragleave") {
      setDragActive(false);
    }
  }, []);

  const processFiles = useCallback(async (files: File[]) => {
    const validFiles: File[] = [];
    const invalidFiles: FileWithError[] = [];

    files.forEach(file => {
      const isValidExtension = /\.(cob|cbl|cobol|cpy|copy|inc)$/i.test(file.name);
      const isValidSize = file.size <= maxFileSizeBytes;

      if (!isValidExtension) {
        invalidFiles.push({
          file,
          error: "Only COBOL files (.cob, .cbl, .cobol, .cpy, .copy, .inc) are supported."
        });
      } else if (!isValidSize) {
        invalidFiles.push({
          file,
          error: `File size exceeds ${maxFileSize}MB limit.`
        });
      } else {
        validFiles.push(file);
      }
    });

    if (invalidFiles.length > 0) {
      setRejectedFiles(prev => [...prev, ...invalidFiles]);
      toast({
        title: `${invalidFiles.length} file(s) rejected`,
        description: invalidFiles.map(f => f.error).join(', '),
        variant: "destructive"
      });
    }

    if (validFiles.length > 0) {
      setIsUploading(true);
      setUploadProgress(0);

      try {
        // Get or generate job ID
        let jobId = SessionManager.getJobId();
        if (!jobId) {
          jobId = SessionManager.generateJobId();
          SessionManager.setJobId(jobId);
        }

        // Show upload progress if enabled
        let progressInterval: NodeJS.Timeout | undefined;
        if (showProgress) {
          progressInterval = setInterval(() => {
            setUploadProgress(prev => Math.min(prev + 10, 90));
          }, 200);
        }

        // Actually upload files to backend
        console.log('Uploading files with jobId:', jobId);
        const uploadResponse = await apiService.uploadFiles(validFiles, jobId);
        console.log('Upload response:', uploadResponse);
        
        // Ensure the jobId is stored
        SessionManager.setJobId(uploadResponse.jobId);
        
        // Complete progress
        if (progressInterval) {
          clearInterval(progressInterval);
        }
        setUploadProgress(100);
        
        setTimeout(() => {
          setIsUploading(false);
          setUploadProgress(0);
        }, 500);

        setUploadedFiles(prev => [...prev, ...validFiles]);
        onFilesSelected(validFiles);
        
        toast({
          title: "Files uploaded successfully",
          description: `${validFiles.length} COBOL file(s) uploaded and ready for analysis.`
        });

      } catch (error) {
        console.error('Upload error:', error);
        setIsUploading(false);
        setUploadProgress(0);
        
        toast({
          title: "Upload failed",
          description: error instanceof Error ? error.message : 'Failed to upload files',
          variant: "destructive"
        });
      }
    }
  }, [onFilesSelected, toast, maxFileSize, maxFileSizeBytes, showProgress]);

  const handleDrop = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setDragActive(false);

    const files = Array.from(e.dataTransfer.files);
    processFiles(files).catch(console.error);
  }, [processFiles]);

  const handleFileInput = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const files = Array.from(e.target.files || []);
    processFiles(files).catch(console.error);
    
    // Reset input to allow re-selecting the same files
    e.target.value = '';
  }, [processFiles]);

  const removeFile = useCallback((index: number) => {
    setUploadedFiles(prev => prev.filter((_, i) => i !== index));
  }, []);

  const clearAllFiles = useCallback(() => {
    setUploadedFiles([]);
    setRejectedFiles([]);
    setUploadProgress(0);
  }, []);

  const clearRejectedFiles = useCallback(() => {
    setRejectedFiles([]);
  }, []);

  const getFileIcon = (fileName: string) => {
    const extension = fileName.split('.').pop()?.toLowerCase();
    if (['cob', 'cbl', 'cobol', 'cpy', 'copy', 'inc'].includes(extension || '')) {
      return <FileText className="w-4 h-4 text-primary" />;
    }
    return <File className="w-4 h-4 text-muted-foreground" />;
  };

  const formatFileSize = (bytes: number) => {
    if (bytes < 1024) return bytes + ' B';
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
    return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
  };

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
              Supports: .cob, .cbl, .cobol, .cpy, .copy, .inc (max {maxFileSize}MB per file)
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

        {/* Upload Progress */}
        {isUploading && showProgress && (
          <div className="mt-6 space-y-2">
            <div className="flex justify-between text-sm">
              <span>Uploading files...</span>
              <span>{uploadProgress}%</span>
            </div>
            <Progress value={uploadProgress} className="h-2" />
          </div>
        )}

        {/* Validation Summary */}
        {(uploadedFiles.length > 0 || rejectedFiles.length > 0) && (
          <div className="mt-6 space-y-3">
            <div className="flex items-center justify-between">
              <h4 className="font-medium flex items-center gap-2">
                <CheckCircle className="w-4 h-4 text-primary" />
                Upload Summary
              </h4>
              {uploadedFiles.length > 0 && (
                <Button
                  variant="outline"
                  size="sm"
                  onClick={clearAllFiles}
                  className="flex items-center gap-1"
                >
                  <Trash2 className="w-3 h-3" />
                  Clear All
                </Button>
              )}
            </div>
            
            <div className="text-sm text-muted-foreground">
              Valid files: {uploadedFiles.length}
              {rejectedFiles.length > 0 && ` • Rejected files: ${rejectedFiles.length}`}
            </div>
          </div>
        )}

        {/* Valid Files List */}
        {uploadedFiles.length > 0 && (
          <div className="mt-4 space-y-2">
            <div className="space-y-2 max-h-40 overflow-y-auto">
              {uploadedFiles.map((file, index) => (
                <div key={index} className="flex items-center justify-between p-3 bg-secondary rounded-md border">
                  <div className="flex items-center gap-3">
                    {getFileIcon(file.name)}
                    <div className="flex flex-col">
                      <span className="text-sm font-medium">{file.name}</span>
                      <span className="text-xs text-muted-foreground">
                        {formatFileSize(file.size)}
                      </span>
                    </div>
                  </div>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => removeFile(index)}
                    className="h-8 w-8 p-0 hover:bg-destructive/10 hover:text-destructive"
                  >
                    <X className="w-4 h-4" />
                  </Button>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Error Messages for Rejected Files */}
        {rejectedFiles.length > 0 && (
          <div className="mt-4 space-y-2">
            <div className="flex items-center justify-between">
              <h5 className="text-sm font-medium text-destructive flex items-center gap-2">
                <AlertCircle className="w-4 h-4" />
                Rejected Files ({rejectedFiles.length})
              </h5>
              <Button
                variant="outline"
                size="sm"
                onClick={clearRejectedFiles}
                className="text-xs"
              >
                Clear
              </Button>
            </div>
            <div className="space-y-2 max-h-32 overflow-y-auto">
              {rejectedFiles.map((item, index) => (
                <Alert key={index} variant="destructive" className="py-2">
                  <AlertDescription className="text-xs">
                    <strong>{item.file.name}</strong>: {item.error}
                  </AlertDescription>
                </Alert>
              ))}
            </div>
          </div>
        )}
      </div>
    </Card>
  );
};