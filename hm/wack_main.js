#!/usr/bin/env node

var child_process = require('child_process');
//var gen_pass = require('./super_gen_pass.js').gen_pass;

var process_f = function (error, stdout, stderr) {
  console.log('stdout: ' + stdout);
  console.log('stderr: ' + stderr);
  if (error !== null) {
    console.log('exec error: ' + error);
  }
};

var process_options = {
  cwd: '/home/danl',
  //cwd: undefined,
  env: process.env,
  customFds: [-1, -1, -1]
};

//child_process.exec('python', ['is_it_a_joke.py'],
//child_process.exec('echo hi | /home/danl/deb/xclip/xclip-0.12/xclip', 
//child_process.exec('echo $DISPLAY | /home/danl/deb/xclip/xclip-0.12/xclip',
child_process.exec(
  // only command "xclip" (which equals "xclip -i") seems to be wack..
  'echo hi | xclip',
  //'echo hi | cat',
  //'xhost',
  //'echo hi | xclip -o',
  process_f, process_options);
//child_process.exec('echo hi');

/*
//domain_name = process.argv.splice(2)[0];
//password = gen_pass(domain_name);
var p = child_process.spawn('/usr/bin/xclip', ['-i']);
//var p = spawn('/usr/bin/xclip', ['-i', '-loops', '1']);
//var p = spawn('python', ['is_it_a_joke.py']);
p.on('exit', function(code, signal) {
  if (code != null) {
    if (code == 0) {
      console.log('in clipboard');
    } else {
      console.log('xclip process exited abnormally with code: ' + code);
    }
  } else {
    console.log('xclip process killed by signal: ' + signal);
  }
});
p.stdout.on('data', function(data) {
  console.log('stdout: ' + data);
});
p.stdout.on('end', function(data) {
  console.log('stdout end');
  child_process.exec('killall xclip');
  // no idea why process hangs in node.js; it works in python
  //p.kill()
  console.log('should be dead');
});
p.stderr.on('data', function(data) {
  console.log('stderr: ' + data);
});
p.stderr.on('end', function(data) {
  console.log('stderr end');
});
//p.stdin.write(password);
p.stdin.write('lalala\n');
p.stdin.end();
*/
