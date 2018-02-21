## Serverless NESL
This project comprises Lambda functions for running the interpreter of NESL language 
(http://www.cs.cmu.edu/~scandal/nesl.html) on AWS Lambda, either directly or via API Gateway
Deployed to AWS Lambda using serverless.com tool:
```
$ sls deploy
```  
Then copy the reported API Gateway URL to the HTML form, as a POST action
with a single input field (e.g., named 'program') containing the input NESL program, e.g.:
```
  {a * a : a in [3, -4, -9, 5] | a > 0};
```
(See example in www.cs.cmu.edu/~scandal/nesl/tutorial2.html)

## Detailed Deployment Instructions
These instructions refer to a standard Ubuntu 16.04 installation (e.g., EC2 Ubuntu 16.04 AMI).

1. Clone this repository and chdir to this folder:
```
$ git clone https://github.com/gblelloch/nesl
$ cd nesl/sls
```
2. Install NodeJS version 8 or above, e.g.:
```
$ curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
```
3. Install the serverless framework:
```
$ sudo npm install -g serverless
```
4. Configure AWS credentials, e.g.:
```
$ sudo apt-get install awscli
$ aws configure
[specify your API KEYs and default region]
$ aws lambda list-functions
[verify that it doesn't fail]
```
5. Deploy the solution:
```
$ sls deploy
[Notice the reported URL of the API Gateway endpoint] 
```

## Test that it works
```
$ apt-get install w3m
$ curl -s -X POST <API-GATEWAY-ENDPOINT> --data body:program=a%3D1%3B | w3m -dump -T text/html
The result of:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

a=1;

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
is:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

a = 1 : int
```

## (Optional) Test the `nesl` function locally
1. Install Python dependencies:
```
$ sudo apt-get install python-pip
$ sudo pip install boto3
```
2. Invoke the function locally via `sls` (expect output similar to the above, but represented as a JSON):
```
$ sls invoke local -f nesl -d '{"program":"a=1;"}'
```

## (Optional) Recompile the interpreter
1. Install Linux dependencies:
```
$ sudo apt-get install byacc lex clisp
```
2. Build binaries (from `nesl` folder of the cloned repository):
```
$ cd nesl
$ make
[ignore warnings]
$ clisp make-server.lisp
```
3. Verify that it worked:
```
$ bin/neslserver
[enter "a=1;<ENTER>" at the prompt (without the ""), observe output similar to above]
```
