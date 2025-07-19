
import React from 'react';
import { Progress } from '@/components/ui/progress';
import { CheckCircle, Circle, Clock } from 'lucide-react';

interface Step {
  id: string;
  label: string;
  description?: string;
}

interface ProgressIndicatorProps {
  steps: Step[];
  currentStep: number;
  className?: string;
}

export const ProgressIndicator = ({ steps, currentStep, className }: ProgressIndicatorProps) => {
  const getProgressPercentage = (step: number) => {
    switch (step) {
      case 0: return 0;   // Upload Assets: 0%
      case 1: return 5;   // Analyze Code: 5%
      case 2: return 60;  // Modernization Plan: 60%
      case 3: return 100; // Implementation: 100%
      default: return 0;
    }
  };
  
  const progressPercentage = getProgressPercentage(currentStep);

  const getStepIcon = (index: number) => {
    if (index < currentStep) {
      return <CheckCircle className="h-5 w-5 text-primary" />;
    } else if (index === currentStep) {
      return <Clock className="h-5 w-5 text-primary animate-pulse" />;
    } else {
      return <Circle className="h-5 w-5 text-muted-foreground" />;
    }
  };

  const getStepStatus = (index: number) => {
    if (index < currentStep) return 'completed';
    if (index === currentStep) return 'current';
    return 'upcoming';
  };

  return (
    <div className={`space-y-6 ${className}`}>
      <div className="space-y-2">
        <div className="flex justify-between text-sm text-muted-foreground">
          <span>Progress</span>
          <span>{Math.round(progressPercentage)}% Complete</span>
        </div>
        <Progress value={progressPercentage} className="h-2" />
      </div>

      <div className="space-y-4">
        {steps.map((step, index) => {
          const status = getStepStatus(index);
          return (
            <div
              key={step.id}
              className={`flex items-start gap-3 p-3 rounded-lg transition-colors ${
                status === 'current'
                  ? 'bg-primary/5 border border-primary/20'
                  : status === 'completed'
                  ? 'bg-muted/50'
                  : 'opacity-60'
              }`}
            >
              {getStepIcon(index)}
              <div className="flex-1 min-w-0">
                <div className={`font-medium ${
                  status === 'current' ? 'text-primary' : 
                  status === 'completed' ? 'text-foreground' : 'text-muted-foreground'
                }`}>
                  {step.label}
                </div>
                {step.description && (
                  <div className="text-sm text-muted-foreground mt-1">
                    {step.description}
                  </div>
                )}
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
};
