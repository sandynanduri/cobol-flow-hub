
import React from 'react';
import { Hero } from '@/components/Hero';
import { NavigationHeader } from '@/components/NavigationHeader';

const Index = () => {
  return (
    <div className="min-h-screen bg-background">
      <NavigationHeader />
      <Hero />
    </div>
  );
};

export default Index;
