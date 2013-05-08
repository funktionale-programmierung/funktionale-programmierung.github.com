var cluster = require('cluster');
var http = require('http');
var numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
    // Fork workers.
    for (var i = 0; i < numCPUs; i++) {
        cluster.fork();
    }

    cluster.on('exit', function(worker, code, signal) {
        console.log('worker ' + worker.process.pid + ' died');
    });
} else {
    var net = require('net');

    var server = net.createServer(function (socket) {
        var leftOver = '';
        socket.on('data', function(data) {
            var str = leftOver + data.toString();
            if (str === "") {
                return;
            }
            var lines = str.split("\n");
            var lastLineOk = str.charAt(str.length - 1) === "\n";
            for (var i = 0; i < lines.length - 1; i++) {
                var s = lines[i];
                if (s === "end") {
                    socket.write("Thank you for using the nodejs doubling service\n");
                    socket.end();
                } else {
                    var j = parseInt(s);
                    if (isNaN(j)) {
                        socket.write("not an int: <" + s + ">\n");
                    } else {
                        socket.write((j * 2) + "\n");
                    }
                }
            }
            leftOver = lastLineOk ? "" : lines[lines.length - 1];
        });
        socket.on('timeout',function(){
            socket.write("timeout");
            socket.end();
        });
        socket.on('error', function() {
        });
    });

    server.listen(44444);

}
