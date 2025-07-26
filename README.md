# COBOL Flow Hub - Complete Modernization Platform

A comprehensive COBOL modernization platform with real backend analysis and AI-powered insights for legacy code transformation.

## 🚀 What's New - Fully Functional Backend!

This project now includes a **complete backend system** that performs actual COBOL file analysis, replacing all mock data with real parsing and insights.

## 🏗️ Architecture

### Frontend (React + TypeScript + Vite)
- **Modern UI** with shadcn/ui components
- **Real-time file upload** with progress tracking
- **Interactive dependency graphs** with ReactFlow
- **Dynamic analysis dashboard** with live data
- **Export functionality** for reports and mappings

### Backend (Node.js + Express)
- **Real COBOL parser** that analyzes file structure, dependencies, and patterns
- **RESTful API** with comprehensive endpoints
- **File upload handling** with validation
- **Graph generation** for dependency visualization
- **Export services** for reports and metadata

## 🛠️ Setup & Installation

### Prerequisites
- Node.js 18+ 
- npm or yarn

### 1. Install Dependencies

```bash
# Install frontend dependencies
npm install

# Install backend dependencies
cd backend
npm install
cd ..
```

### 2. Start Both Servers

```bash
# Terminal 1: Start Backend (Port 3001)
cd backend
npm start

# Terminal 2: Start Frontend (Port 5173) 
npm run dev
```

### 3. Access the Application
- **Frontend**: http://localhost:5173
- **Backend API**: http://localhost:3001/api
- **Health Check**: http://localhost:3001/api/health

## 📊 Real Backend Features

### COBOL File Analysis Engine
- **File Type Detection**: Automatically identifies programs vs copybooks
- **LOC Counting**: Accurate lines of code analysis (excluding comments)
- **Dependency Extraction**: Finds COPY and CALL statements
- **Pattern Recognition**: Detects PERFORM THRU, file I/O, and complexity patterns
- **Missing Dependencies**: Identifies referenced but missing files

### API Endpoints

#### Upload & Analysis
```
POST /api/upload                 - Upload COBOL files
POST /api/analyze/summary         - Analyze uploaded files  
GET  /api/analyze/job/:jobId      - Get existing analysis
POST /api/analyze/file            - Analyze individual file
POST /api/analyze/convert         - Generate conversion plan
```

#### Dependency Graphs
```
GET  /api/graph/:jobId            - Get dependency graph data
POST /api/graph/custom            - Custom graph with layout options
```

#### Export Services
```
GET  /api/export/metadata/:jobId  - Export analysis metadata
GET  /api/export/report/:jobId    - Generate comprehensive report
GET  /api/export/mapping/:jobId   - COBOL to language mapping table
POST /api/export/custom           - Custom export with sections
```

## 🎯 Complete User Journey

### 1. Upload Files
- Drag & drop COBOL files (.cob, .cbl, .cobol, .cpy, .copy, .inc)
- Real-time validation and error handling
- Files are stored in backend with unique job IDs

### 2. Analysis Process
- Click "Start Modernization Analysis"
- Backend parses files and extracts:
  - File classifications and structure
  - Dependencies and relationships  
  - Code patterns and complexity
  - Missing files and issues

### 3. View Results
- **File Classification**: Programs, copybooks, LOC counts
- **Dependency Graph**: Interactive visualization with ReactFlow
- **AI Insights**: PERFORM patterns, file I/O, complexity scoring
- **Missing Files**: Alerts for unresolved dependencies

### 4. Export & Download
- **Metadata Export**: Complete analysis as JSON
- **Mapping Tables**: COBOL → Java/Python conversion guides
- **Comprehensive Reports**: Executive summaries and technical details

## 📁 Sample COBOL Files

The backend includes sample files for testing:

```
backend/samples/
├── EMPLOYEE.CBL           # Main program with payroll logic
├── EMPLOYEE-RECORD.cpy    # Employee data structure
├── EMPLOYEE-CONSTANTS.cpy # Constants and error messages  
└── INVENTORY.CBL          # Inventory management system
```

### Test the System
1. Upload `EMPLOYEE.CBL` from the samples folder
2. Run analysis to see real results:
   - **Dependencies**: COPY statements and external CALLs
   - **Missing Files**: EMPLOYEE-RECORD.cpy, TAX-CALC, AUDIT-LOG
   - **Patterns**: PERFORM statements, file I/O operations
   - **Graph**: Visual dependency relationships

## 🧠 Real Analysis Capabilities

### Code Structure Analysis
- **Division Detection**: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- **Paragraph Counting**: Automatic procedure detection
- **Section Analysis**: Program organization insights

### Dependency Mapping
- **COPY Statements**: Data structure dependencies
- **CALL Statements**: External program dependencies  
- **Missing References**: Unresolved dependencies with alerts

### Pattern Recognition
- **PERFORM THRU**: Legacy control flow patterns
- **File I/O**: OPEN, READ, WRITE, CLOSE operations
- **Data Access**: VSAM vs Sequential file detection
- **Conditional Logic**: IF, EVALUATE, WHEN statement counting

### Complexity Scoring
- **LOC-based**: Lines of code analysis
- **Dependency-based**: Number of external references
- **Pattern-based**: Legacy constructs and complexity indicators
- **Final Score**: LOW, MEDIUM, HIGH complexity ratings

## 📊 Dashboard Features

### Real-Time Statistics
- **Files Analyzed**: Actual count from backend
- **Lines of Code**: Real LOC from parser
- **Complexity Score**: Calculated complexity rating
- **Estimated Effort**: Algorithm-based effort calculation

### Interactive Elements
- **Dependency Graph**: ReactFlow visualization with real data
- **Action Buttons**: Working exports and conversion planning
- **Progress Tracking**: 4-step modernization process
- **Error Handling**: Comprehensive error states and recovery

## 🔧 API Service Layer

### Frontend Integration
```typescript
// Real API calls replace all mock data
import { apiService, SessionManager } from '@/services/api';

// Upload files
const result = await apiService.uploadFiles(files, jobId);

// Analyze COBOL
const analysis = await apiService.analyzeFiles(jobId);

// Get dependency graph  
const graph = await apiService.getGraphData(jobId);

// Export reports
const blob = await apiService.exportMetadata(jobId);
```

### Session Management
- **Job ID Generation**: Unique identifiers for each analysis
- **Local Storage**: Caching for performance
- **State Persistence**: Session recovery across page refreshes

## 📈 Performance & Scalability

### Backend Optimizations
- **Streaming File Upload**: Handles large COBOL files efficiently
- **Caching**: Analysis results stored for quick retrieval
- **Error Recovery**: Robust error handling and validation

### Frontend Optimizations  
- **Progressive Loading**: Components load data as needed
- **Error Boundaries**: Graceful degradation on failures
- **Responsive Design**: Works on desktop and mobile

## 🧪 Testing

### Backend Testing
```bash
cd backend
npm test                    # Run backend tests
```

### Frontend Testing
```bash
npm run test               # Run frontend tests
npm run test:coverage     # Coverage reports
```

### Manual Testing
1. Upload sample COBOL files
2. Verify real analysis results
3. Test dependency graph rendering
4. Check export functionality
5. Validate error handling

## 🚀 Deployment

### Production Build
```bash
# Build frontend
npm run build

# Start production backend
cd backend  
npm start
```

### Environment Variables
```bash
# Backend (.env)
PORT=3001
NODE_ENV=production
UPLOAD_DIR=./uploads

# Frontend (.env)
VITE_API_URL=http://localhost:3001/api
```

## 🤝 Contributing

### Adding New Analysis Features
1. Update `CobolParser.js` with new patterns
2. Add corresponding API endpoints
3. Update frontend interfaces
4. Add tests and documentation

### Adding New Export Formats
1. Implement converter in `routes/export.js`
2. Add frontend download handlers
3. Update UI with new export options

## 📞 Support

- **Issues**: Report bugs and feature requests
- **Documentation**: Comprehensive API docs available
- **Examples**: Sample COBOL files and test cases included

---

## 🎉 Achievement Summary

✅ **Complete Backend Integration** - Real COBOL parsing and analysis
✅ **Dynamic Frontend** - All mock data replaced with live API calls  
✅ **File Upload System** - Working file handling with validation
✅ **Dependency Visualization** - Interactive graphs with real data
✅ **Export Functionality** - Working downloads and reports
✅ **Error Handling** - Comprehensive error states and recovery
✅ **Session Management** - Persistent state across page loads
✅ **Sample Files** - Ready-to-test COBOL examples

**The COBOL Flow Hub is now a fully functional modernization platform!** 🚀
