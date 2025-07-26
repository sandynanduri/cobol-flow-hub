const express = require('express');
const cors = require('cors');
const multer = require('multer');
const path = require('path');
const fs = require('fs-extra');
const { v4: uuidv4 } = require('uuid');

const app = express();
const PORT = process.env.PORT || 3001;

// Middleware
app.use(cors());
app.use(express.json());
app.use(express.urlencoded({ extended: true }));

// Ensure uploads directory exists
const UPLOADS_DIR = path.join(__dirname, 'uploads');
fs.ensureDirSync(UPLOADS_DIR);

// Configure multer for file uploads
const storage = multer.diskStorage({
  destination: (req, file, cb) => {
    // We'll handle jobId in the route handler instead
    const tempDir = path.join(UPLOADS_DIR, 'temp');
    fs.ensureDirSync(tempDir);
    cb(null, tempDir);
  },
  filename: (req, file, cb) => {
    // Add timestamp to prevent conflicts
    const timestamp = Date.now();
    cb(null, `${timestamp}-${file.originalname}`);
  }
});

const fileFilter = (req, file, cb) => {
  const allowedExtensions = /\.(cob|cbl|cobol|cpy|copy|inc)$/i;
  if (allowedExtensions.test(file.originalname)) {
    cb(null, true);
  } else {
    cb(new Error('Only COBOL files are allowed'), false);
  }
};

const upload = multer({ 
  storage,
  fileFilter,
  limits: {
    fileSize: 5 * 1024 * 1024 // 5MB limit
  }
});

// Import route modules
const analyzeRoutes = require('./routes/analyze');
const graphRoutes = require('./routes/graph');
const exportRoutes = require('./routes/export');

// API Routes
app.use('/api/analyze', analyzeRoutes);
app.use('/api/graph', graphRoutes);
app.use('/api/export', exportRoutes);

// File upload endpoint
app.post('/api/upload', upload.array('files'), async (req, res) => {
  try {
    if (!req.files || req.files.length === 0) {
      return res.status(400).json({ error: 'No files uploaded' });
    }

    // Get or generate jobId
    const jobId = req.body.jobId || uuidv4();
    const jobDir = path.join(UPLOADS_DIR, jobId);
    fs.ensureDirSync(jobDir);

    // Move files from temp to job directory
    const files = [];
    for (const file of req.files) {
      const originalName = file.originalname;
      const newPath = path.join(jobDir, originalName);
      
      try {
        // Check if destination exists and remove it
        if (fs.existsSync(newPath)) {
          fs.removeSync(newPath);
        }
        
        // Move file from temp to job directory
        fs.moveSync(file.path, newPath);
        
        files.push({
          originalName: originalName,
          filename: originalName,
          size: file.size,
          path: newPath
        });
      } catch (moveError) {
        console.error(`Failed to move file ${originalName}:`, moveError);
        // Fallback: copy instead of move
        try {
          fs.copySync(file.path, newPath);
          fs.removeSync(file.path); // Remove temp file
          
          files.push({
            originalName: originalName,
            filename: originalName,
            size: file.size,
            path: newPath
          });
        } catch (copyError) {
          console.error(`Failed to copy file ${originalName}:`, copyError);
          throw new Error(`Failed to process file ${originalName}: ${copyError.message}`);
        }
      }
    }

    console.log(`Files uploaded successfully for job ${jobId}:`, files.map(f => f.originalName));

    res.json({
      jobId,
      message: 'Files uploaded successfully',
      files: files
    });
  } catch (error) {
    console.error('Upload error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Health check endpoint
app.get('/api/health', (req, res) => {
  res.json({ status: 'OK', message: 'COBOL Flow Backend is running' });
});

// Error handling middleware
app.use((error, req, res, next) => {
  if (error instanceof multer.MulterError) {
    if (error.code === 'LIMIT_FILE_SIZE') {
      return res.status(400).json({ error: 'File size too large. Maximum 5MB allowed.' });
    }
  }
  
  console.error('Server error:', error);
  res.status(500).json({ error: error.message || 'Internal server error' });
});

app.listen(PORT, () => {
  console.log(`🚀 COBOL Flow Backend running on http://localhost:${PORT}`);
  console.log(`📁 Upload directory: ${UPLOADS_DIR}`);
});

module.exports = app; 