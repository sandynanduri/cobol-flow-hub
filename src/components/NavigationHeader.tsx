
import React from 'react';
import { Button } from '@/components/ui/button';
import { ArrowLeft, Home } from 'lucide-react';
import { Link, useNavigate, useLocation } from 'react-router-dom';

interface NavigationHeaderProps {
  showBackButton?: boolean;
  title?: string;
}

export const NavigationHeader = ({ showBackButton = false, title }: NavigationHeaderProps) => {
  const navigate = useNavigate();
  const location = useLocation();

  const handleBack = () => {
    navigate(-1);
  };

  const getPageTitle = () => {
    if (title) return title;
    
    switch (location.pathname) {
      case '/':
        return 'COBOL Modernization Platform';
      case '/upload':
        return 'Upload Assets';
      default:
        return 'COBOL Modernization';
    }
  };

  return (
    <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="container flex h-14 items-center">
        <div className="flex items-center gap-4">
          {showBackButton && (
            <Button
              variant="ghost"
              size="sm"
              onClick={handleBack}
              className="flex items-center gap-2"
            >
              <ArrowLeft className="h-4 w-4" />
              Back
            </Button>
          )}
          
          <Link to="/" className="flex items-center gap-2 hover:opacity-80 transition-opacity">
            <Home className="h-5 w-5" />
            <span className="font-semibold">{getPageTitle()}</span>
          </Link>
        </div>

        <div className="flex-1" />

        <nav className="flex items-center gap-4">
          <Link to="/">
            <Button variant="ghost" size="sm">
              Home
            </Button>
          </Link>
          <Link to="/upload">
            <Button variant="ghost" size="sm">
              Upload
            </Button>
          </Link>
        </nav>
      </div>
    </header>
  );
};
