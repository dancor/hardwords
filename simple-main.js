var child_process = require('child_process');
var gen_pass = require('./super-gen-pass.js').gen_pass;
var util = require('util');

domain_name = process.argv.splice(2)[0];
password = gen_pass(domain_name);
util.print(password);
