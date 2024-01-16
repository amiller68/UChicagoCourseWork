const express = require("express");
const fs = require('fs');
const https = require('https');
const app = express();

// Get our server's path
const path = require('path');
const dir = path.join(__dirname, '');

app.use(express.static(dir));

app.get("/", (req, res) => {
  res.sendFile('index.html', {root: '/'})
});

// Start listening for insecure connections
app.listen(
	7000,
	() => console.log("Server listening for insecure connections on port 7000...")
);

// Associate our cert and key with our server, and start listening for secure connections
https.createServer(
	{
 		key: fs.readFileSync('server.key'),
  		cert: fs.readFileSync('server.cert')
	}, 
	app
).listen(
	8000,
	() => console.log("Server listening for secure connections on port 8000...")
)
