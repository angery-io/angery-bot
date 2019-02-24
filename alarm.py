import json
from dateutil.parser import parse
from urllib.request import Request, urlopen
import os


SLACK_POSTMESSAGE_API = "https://slack.com/api/chat.postMessage"
try:
    TOKEN = os.environ["TOKEN"]
    CHANNEL = os.environ["CHANNEL"]
except KeyError:
    raise RuntimeError("TOKEN or CHANNEL environment variable is not defined")


def lambda_handler(event, context):
    sns_message = json.loads(event["Records"][0]["Sns"]["Message"])

    alarm = sns_message["NewStateValue"] == "ALARM"
    text = sns_message["NewStateReason"]
    color = "#ff0000" if alarm else "#00ff00"
    pretext = "Hey, looks like I am broken" if alarm else "I am OK now!"
    time = int(parse(sns_message["StateChangeTime"]).timestamp())
    message = {
        "channel": CHANNEL,
        "attachments": [
            {
                "fallback": "Alarm for Angery Bot raised",
                "color": color,
                "pretext": pretext,
                "author_name": "Angery Bot Alarm",
                "author_link": "https://console.aws.amazon.com/cloudwatch/home",
                "title": "AngeryBot API 5xx errors",
                "title_link": "https://console.aws.amazon.com/cloudwatch/home",
                "text": text,
                # "thumb_url": "https://i.pinimg.com/236x/af/aa/b0/afaab0eeec70081a60ac46a08a85d131.jpg",
                "ts": time,
            }
        ],
    }
    request = Request(
        SLACK_POSTMESSAGE_API,
        data=json.dumps(message).encode(),
        headers={
            "Content-type": "application/json",
            "Authorization": f"Bearer {TOKEN}",
        },
        method="POST",
    )
    urlopen(request)
    return {}
