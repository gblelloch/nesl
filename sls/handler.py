# Lambda functions for running the interpreter of NESL language on AWS Lambda, either directly or via API Gateway
# See README for more details

import os, subprocess
import urllib, base64
import logging
import re, json
import boto3, botocore

logger = logging.getLogger()
logger.setLevel(logging.INFO)

# Lambda function that acts as an HTTP handler invoked via API Gateway, wrapping <nesl> function
def neslapi(event, context):
    # retrieve program text (assuming it is received via API gateway, and hence is quoted)
    # the request body is supposed to be in the form "program=<quoted program text>"
    # notice that the program may contain '=', hence unquoting should be done after parsing
    _,prog = event["body"].split('=')
    prog = urllib.unquote_plus(prog)
    logger.info('Program:\n' + str(prog))

    # invoke <nesl> lambda function to actually run the interpreter
    out, err = "NONE", ""
    lambda_client = boto3.client('lambda')
    try: 
        # HACK: we assume that the public name of the 'nesl' function is the same as of *this* one,
        # but without the "api" at the end
        neslfn = os.environ["AWS_LAMBDA_FUNCTION_NAME"][:-3]
        resp = lambda_client.invoke(FunctionName=neslfn, Payload=json.dumps({"program":prog}), LogType="Tail")
        logger.info('Lambda function invocation logs: \n' + base64.b64decode(resp["LogResult"]))
        res = resp['Payload'].read().decode()
        logger.info('Payload returned from the Lambda invocation: ' + res)
        if "FunctionError" in resp:
            logger.info('Lambda invocation returned function error: ' + resp["FunctionError"])
            err = "Error running the NESL interpreter (exceeded resource limits?)"
        else:
            out, err = json.loads(res)["out"], json.loads(res)["err"]
    except botocore.exceptions.ClientError as e:
        logger.info('Lambda invocation exception: ' + str(e))
        err = "Error running the NESL interpreter." 

    # generate output HTML
    body = "<b> The result of: </b>\n<hr>\n<pre>\n" + prog + "\n</pre>\n<hr>\n"
    body += "<b> is: </b>\n<hr>\n<pre>\n" + out + "\n</pre>\n"
    if len(err) > 0:
        body += "<hr>\n<b> ERROR: </b>\n<hr>\n<pre>\n" + err + "\n</pre>\n"

    return { "statusCode": 200,
             "body"      : "<html> <body>\n" + body + "</body> </html>\n",
             "headers"   : { 'Content-Type': 'text/html' } }

# Lambda function running the NESL interpreter on the received 'program' argument
# returns the resulting output and error (if occured)
def nesl(event, context):
    prog = event['program']
 
    # Update dynamic libraries PATH to include libraries included with this lambda function
    os.environ["LD_LIBRARY_PATH"] = './lib'

    # Call the NESL interpreter, feed input from <prog>, decode output
    p = subprocess.Popen("bin/neslserver", stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate(input=prog.encode('latin-1'))
    #out, err = aout.decode(), aerr.decode()
    logger.info('Interpreter output: \n' + out)
    logger.info('Interpreter error: \n' + err)

    return {"out": out, "err": err}

