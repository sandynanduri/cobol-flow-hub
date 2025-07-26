import React from 'react';
import { ArrowRight, Zap, Shield, TrendingUp } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { useNavigate } from 'react-router-dom';

export const Hero: React.FC = () => {
  const navigate = useNavigate();

  const handleGetStarted = () => {
    navigate('/upload');
  };

  return (
    <div className="relative overflow-hidden bg-gradient-hero">
      {/* Background decoration */}
      <div className="absolute inset-0 bg-grid-pattern opacity-5"></div>
      <div className="absolute -top-40 -right-40 w-80 h-80 bg-primary/20 rounded-full blur-3xl"></div>
      <div className="absolute -bottom-40 -left-40 w-80 h-80 bg-accent/20 rounded-full blur-3xl"></div>
      
      <div className="relative max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8 lg:py-12 min-h-screen flex items-center">
        <div className="text-center space-y-6 animate-fade-in w-full">
          {/* Badge */}
          <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full bg-primary/10 border border-primary/20 text-primary font-medium text-sm">
            <Zap className="w-4 h-4" />
            Legacy COBOL Modernization Platform
          </div>

          {/* Main heading */}
          <div className="space-y-3">
            <h1 className="text-3xl md:text-5xl lg:text-6xl font-bold text-foreground tracking-tight">
              Transform Your
              <span className="block bg-gradient-primary bg-clip-text text-transparent">
                COBOL Legacy
              </span>
            </h1>
            <p className="text-lg md:text-xl text-muted-foreground max-w-3xl mx-auto leading-relaxed">
              Modernize your COBOL applications with AI-powered analysis, automated migration,
              and enterprise-grade security. Start your digital transformation today.
            </p>
          </div>

          {/* CTA Buttons */}
          <div className="flex justify-center items-center">
            <Button variant="hero" size="lg" className="group" onClick={handleGetStarted}>
              Get Started Now
              <ArrowRight className="w-4 h-4 transition-transform group-hover:translate-x-1" />
            </Button>
          </div>

          {/* Feature highlights */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6 pt-8 max-w-4xl mx-auto">
            <div className="flex flex-col items-center space-y-2 p-4 rounded-lg bg-card/50 backdrop-blur-sm border border-border/50 hover:shadow-card transition-all duration-300 animate-scale-in">
              <div className="w-10 h-10 rounded-full bg-primary/10 flex items-center justify-center">
                <Zap className="w-5 h-5 text-primary" />
              </div>
              <h3 className="font-semibold text-foreground text-sm">AI-Powered Analysis</h3>
              <p className="text-muted-foreground text-xs text-center">
                Advanced algorithms analyze your COBOL codebase and recommend optimal modernization strategies
              </p>
            </div>

            <div className="flex flex-col items-center space-y-2 p-4 rounded-lg bg-card/50 backdrop-blur-sm border border-border/50 hover:shadow-card transition-all duration-300 animate-scale-in">
              <div className="w-10 h-10 rounded-full bg-accent/10 flex items-center justify-center">
                <Shield className="w-5 h-5 text-accent" />
              </div>
              <h3 className="font-semibold text-foreground text-sm">Enterprise Security</h3>
              <p className="text-muted-foreground text-xs text-center">
                Bank-grade security ensures your sensitive legacy code remains protected throughout the process
              </p>
            </div>

            <div className="flex flex-col items-center space-y-2 p-4 rounded-lg bg-card/50 backdrop-blur-sm border border-border/50 hover:shadow-card transition-all duration-300 animate-scale-in">
              <div className="w-10 h-10 rounded-full bg-primary/10 flex items-center justify-center">
                <TrendingUp className="w-5 h-5 text-primary" />
              </div>
              <h3 className="font-semibold text-foreground text-sm">Accelerated Migration</h3>
              <p className="text-muted-foreground text-xs text-center">
                Reduce migration time by 70% with automated code transformation and testing frameworks
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};