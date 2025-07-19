import React, { useState } from 'react';
import { Github, GitBranch, ExternalLink, CheckCircle, X } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Card } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { useToast } from '@/hooks/use-toast';

interface RepositoryLinkProps {
  onRepositoryAdded: (url: string) => void;
}

export const RepositoryLink: React.FC<RepositoryLinkProps> = ({ onRepositoryAdded }) => {
  const [repositoryUrl, setRepositoryUrl] = useState('');
  const [isValidUrl, setIsValidUrl] = useState(false);
  const [addedRepositories, setAddedRepositories] = useState<string[]>([]);
  const { toast } = useToast();

  const validateUrl = (url: string) => {
    const gitUrlPattern = /^https?:\/\/(github\.com|bitbucket\.org|gitlab\.com)\/[\w\-._]+\/[\w\-._]+/;
    return gitUrlPattern.test(url);
  };

  const handleUrlChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const url = e.target.value;
    setRepositoryUrl(url);
    setIsValidUrl(validateUrl(url));
  };

  const handleAddRepository = () => {
    if (isValidUrl && repositoryUrl) {
      if (addedRepositories.includes(repositoryUrl)) {
        toast({
          title: "Repository already added",
          description: "This repository URL has already been added.",
          variant: "destructive"
        });
        return;
      }

      setAddedRepositories(prev => [...prev, repositoryUrl]);
      onRepositoryAdded(repositoryUrl);
      toast({
        title: "Repository added successfully",
        description: "Repository is ready for COBOL modernization analysis."
      });
      setRepositoryUrl('');
      setIsValidUrl(false);
    }
  };

  const removeRepository = (url: string) => {
    setAddedRepositories(prev => prev.filter(repo => repo !== url));
  };

  const getRepositoryIcon = (url: string) => {
    if (url.includes('github.com')) return Github;
    if (url.includes('bitbucket.org')) return GitBranch;
    if (url.includes('gitlab.com')) return GitBranch;
    return Github;
  };

  const getRepositoryName = (url: string) => {
    try {
      const urlObj = new URL(url);
      const pathParts = urlObj.pathname.split('/').filter(Boolean);
      if (pathParts.length >= 2) {
        return `${pathParts[0]}/${pathParts[1]}`;
      }
      return url;
    } catch {
      return url;
    }
  };

  return (
    <Card className="p-8 bg-gradient-card backdrop-blur-sm border transition-all duration-300 hover:shadow-card">
      <div className="space-y-6">
        <div className="text-center space-y-4">
          <div className="mx-auto w-16 h-16 rounded-full bg-gradient-primary flex items-center justify-center">
            <Github className="w-8 h-8 text-primary-foreground" />
          </div>
          
          <div className="space-y-2">
            <h3 className="text-xl font-semibold text-foreground">
              Connect Repository
            </h3>
            <p className="text-muted-foreground">
              Link your GitHub, Bitbucket, or GitLab repository
            </p>
            <p className="text-sm text-muted-foreground">
              We'll analyze your COBOL codebase for modernization opportunities
            </p>
          </div>
        </div>

        <div className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="repository-url">Repository URL</Label>
            <div className="relative">
              <Input
                id="repository-url"
                type="url"
                placeholder="https://github.com/username/repository"
                value={repositoryUrl}
                onChange={handleUrlChange}
                className={`pr-12 transition-all duration-200 ${
                  repositoryUrl && (isValidUrl ? 'border-primary' : 'border-destructive')
                }`}
              />
              {repositoryUrl && (
                <div className="absolute right-3 top-1/2 transform -translate-y-1/2">
                  {isValidUrl ? (
                    <CheckCircle className="w-4 h-4 text-primary" />
                  ) : (
                    <ExternalLink className="w-4 h-4 text-destructive" />
                  )}
                </div>
              )}
            </div>
            {repositoryUrl && !isValidUrl && (
              <p className="text-sm text-destructive">
                Please enter a valid GitHub, Bitbucket, or GitLab URL
              </p>
            )}
          </div>

          <Button 
            onClick={handleAddRepository} 
            disabled={!isValidUrl}
            variant="hero"
            size="lg"
            className="w-full"
          >
            Add Repository
          </Button>
        </div>

        {addedRepositories.length > 0 && (
          <div className="space-y-3">
            <h4 className="font-medium flex items-center gap-2">
              <CheckCircle className="w-4 h-4 text-primary" />
              Added Repositories ({addedRepositories.length})
            </h4>
            <div className="space-y-2">
              {addedRepositories.map((url, index) => {
                const Icon = getRepositoryIcon(url);
                return (
                  <div key={index} className="flex items-center justify-between p-3 bg-secondary rounded-md">
                    <div className="flex items-center gap-3">
                      <Icon className="w-4 h-4 text-primary" />
                      <div>
                        <p className="text-sm font-medium">{getRepositoryName(url)}</p>
                        <p className="text-xs text-muted-foreground truncate max-w-xs">{url}</p>
                      </div>
                    </div>
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => removeRepository(url)}
                      className="h-8 w-8 p-0"
                    >
                      <X className="w-4 h-4" />
                    </Button>
                  </div>
                );
              })}
            </div>
          </div>
        )}
      </div>
    </Card>
  );
};