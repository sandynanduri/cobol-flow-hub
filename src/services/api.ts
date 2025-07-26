const API_BASE_URL = 'http://localhost:3001/api';

export interface FileUploadResponse {
  jobId: string;
  message: string;
  files: Array<{
    originalName: string;
    filename: string;
    size: number;
    path: string;
  }>;
}

export interface AnalysisResponse {
  jobId: string;
  timestamp: string;
  fileClassification: Array<{
    category: string;
    count: number;
    files: string[];
  }>;
  extractedInsights: Array<{
    type: string;
    value: string;
  }>;
  missingFiles: string[];
  complexity: string;
  estimatedEffort: string;
  summary: any;
  files: any[];
}

export interface GraphData {
  jobId: string;
  timestamp: string;
  nodes: Array<{
    id: string;
    type: string;
    position: { x: number; y: number };
    data: { label: string };
    style: any;
  }>;
  edges: Array<{
    id: string;
    source: string;
    target: string;
    type: string;
    style: any;
    label: string;
    labelStyle: any;
  }>;
  metadata: {
    totalNodes: number;
    totalEdges: number;
    missingNodes: number;
  };
}

class ApiService {
  private baseUrl: string;

  constructor(baseUrl: string = API_BASE_URL) {
    this.baseUrl = baseUrl;
  }

  /**
   * Check if backend is available
   */
  async healthCheck(): Promise<boolean> {
    try {
      const response = await fetch(`${this.baseUrl}/health`);
      return response.ok;
    } catch (error) {
      console.error('Backend health check failed:', error);
      return false;
    }
  }

  /**
   * Upload COBOL files to backend
   */
  async uploadFiles(files: File[], jobId?: string): Promise<FileUploadResponse> {
    const formData = new FormData();
    
    files.forEach(file => {
      formData.append('files', file);
    });
    
    if (jobId) {
      formData.append('jobId', jobId);
    }

    console.log('Uploading to:', `${this.baseUrl}/upload`);
    console.log('FormData entries:', Array.from(formData.entries()));

    const response = await fetch(`${this.baseUrl}/upload`, {
      method: 'POST',
      body: formData,
    });

    console.log('Upload response status:', response.status);
    console.log('Upload response headers:', response.headers);

    if (!response.ok) {
      const errorText = await response.text();
      console.error('Upload error response:', errorText);
      let error;
      try {
        error = JSON.parse(errorText);
      } catch {
        error = { error: errorText || 'Upload failed' };
      }
      throw new Error(error.error || 'Upload failed');
    }

    const result = await response.json();
    console.log('Upload success result:', result);
    return result;
  }

  /**
   * Analyze uploaded COBOL files
   */
  async analyzeFiles(jobId: string): Promise<AnalysisResponse> {
    console.log('Analyzing files for jobId:', jobId);
    console.log('Analysis URL:', `${this.baseUrl}/analyze/summary`);
    
    const response = await fetch(`${this.baseUrl}/analyze/summary`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ jobId }),
    });

    console.log('Analysis response status:', response.status);
    console.log('Analysis response URL:', response.url);

    if (!response.ok) {
      const errorText = await response.text();
      console.error('Analysis error response:', errorText);
      let error;
      try {
        error = JSON.parse(errorText);
      } catch {
        error = { error: errorText || 'Analysis failed' };
      }
      throw new Error(error.error || 'Analysis failed');
    }

    const result = await response.json();
    console.log('Analysis success result:', result);
    return result;
  }

  /**
   * Get existing analysis results
   */
  async getAnalysis(jobId: string): Promise<AnalysisResponse> {
    const response = await fetch(`${this.baseUrl}/analyze/job/${jobId}`);

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Failed to get analysis');
    }

    return response.json();
  }

  /**
   * Get dependency graph data
   */
  async getGraphData(jobId: string): Promise<GraphData> {
    const response = await fetch(`${this.baseUrl}/graph/${jobId}`);

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Failed to get graph data');
    }

    return response.json();
  }

  /**
   * Analyze individual file
   */
  async analyzeFile(jobId: string, fileName: string): Promise<any> {
    const response = await fetch(`${this.baseUrl}/analyze/file`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ jobId, fileName }),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'File analysis failed');
    }

    return response.json();
  }

  /**
   * Convert COBOL to target language
   */
  async convertToLanguage(jobId: string, targetLanguage: string): Promise<any> {
    const response = await fetch(`${this.baseUrl}/analyze/convert`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ jobId, targetLanguage }),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Conversion failed');
    }

    return response.json();
  }

  /**
   * Export analysis metadata
   */
  async exportMetadata(jobId: string): Promise<Blob> {
    const response = await fetch(`${this.baseUrl}/export/metadata/${jobId}`);

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Export failed');
    }

    return response.blob();
  }

  /**
   * Generate analysis report
   */
  async generateReport(jobId: string, format: 'json' | 'csv' | 'txt' = 'json'): Promise<Blob> {
    const response = await fetch(`${this.baseUrl}/export/report/${jobId}?format=${format}`);

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Report generation failed');
    }

    return response.blob();
  }

  /**
   * Get COBOL to language mapping
   */
  async getMappingTable(jobId: string, targetLanguage: string = 'java'): Promise<Blob> {
    const response = await fetch(`${this.baseUrl}/export/mapping/${jobId}?targetLanguage=${targetLanguage}`);

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Mapping generation failed');
    }

    return response.blob();
  }

  /**
   * Download file from blob
   */
  downloadFile(blob: Blob, filename: string): void {
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
    document.body.removeChild(a);
  }
}

// Create singleton instance
export const apiService = new ApiService();

// Storage for current job session
export class SessionManager {
  private static readonly JOB_ID_KEY = 'cobol-flow-job-id';
  private static readonly ANALYSIS_CACHE_KEY = 'cobol-flow-analysis-cache';

  static setJobId(jobId: string): void {
    localStorage.setItem(this.JOB_ID_KEY, jobId);
  }

  static getJobId(): string | null {
    return localStorage.getItem(this.JOB_ID_KEY);
  }

  static clearJobId(): void {
    localStorage.removeItem(this.JOB_ID_KEY);
    localStorage.removeItem(this.ANALYSIS_CACHE_KEY);
  }

  static cacheAnalysis(analysis: AnalysisResponse): void {
    localStorage.setItem(this.ANALYSIS_CACHE_KEY, JSON.stringify(analysis));
  }

  static getCachedAnalysis(): AnalysisResponse | null {
    const cached = localStorage.getItem(this.ANALYSIS_CACHE_KEY);
    return cached ? JSON.parse(cached) : null;
  }

  static generateJobId(): string {
    const timestamp = new Date().toISOString().slice(0, 19).replace(/[:-]/g, '');
    const random = Math.random().toString(36).substring(2, 8);
    return `job_${timestamp}_${random}`;
  }
}

export default apiService; 