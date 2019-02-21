#!/usr/bin/python3
import os
import sys
import json
from random import choice
from urllib.request import urlopen, Request
import hmac
import hashlib
from time import time


SLACK_POSTMESSAGE_API = "https://slack.com/api/chat.postMessage"
SLACK_TIMESTAMP = "X-Slack-Request-Timestamp"
SLACK_SIGNATURE = "X-Slack-Signature"

ANGERY_SUPPRESSOR = ("no_react", "noreact", "nobot", "no_bot")
ANGERY_TRIGGER = (
    "angery",
    "angry",
    "anger",
    ":frowning:",
    ":anger:",
    ":angry:",
    "aggravating",
)
ANGERY_RESOURCES = os.path.join("resources", "angery.txt")


try:
    TOKEN = os.environ["SLACK_TOKEN"]
except KeyError:
    raise RuntimeError("SLACK_TOKEN environment variable is not set.")


def verify_request_comes_from_slack(event):
    try:
        SIGNING_SECRET = os.environ["SLACK_SIGNING_SECRET"].encode()
    except KeyError:
        raise RuntimeError("SLACK_SIGNING_SECRET environment variable is not set.")
        
    headers = event.get('headers', {})
    if SLACK_TIMESTAMP not in headers or SLACK_SIGNATURE not in headers:
        raise RuntimeError('Missing Slack verification headers')
    
    timestamp = headers[SLACK_TIMESTAMP]
    if time() - int(timestamp) > 60:
        raise RuntimeError('Slack message too old - possible replay')
    
    raw_body = event['body']
    signature_base = f'v0:{timestamp}:{raw_body}'.encode()
    signature = 'v0=' + hmac.new(SIGNING_SECRET, signature_base, hashlib.sha256).hexdigest()
    if signature != headers[SLACK_SIGNATURE]:
        raise RuntimeError('Slack signature not matching')


def lambda_handler(event, context):
    verify_request_comes_from_slack(event)
    
    body = json.loads(event["body"])
    if "challenge" in body:
        return {"body": body["challenge"]}

    else:
        event = body.get("event", {})
        channel = event.get("channel", "")
        text = event.get("text", "").lower()
        item = event.get("item", {})
        reaction = event.get("reaction", "")
        channel = channel or item.get("channel", "")
        search_space = text + reaction

        if (
            search_space
            and not any(suppressor in search_space for suppressor in ANGERY_SUPPRESSOR)
            and any(trigger in search_space for trigger in ANGERY_TRIGGER)
        ):
            with open(ANGERY_RESOURCES, "r") as f:
                imgs = f.readlines()
            img = choice(imgs)[:-1]

            payload = {
                "channel": channel,
                "attachments": [{
                    "image_url": img,
                    "fallback": "Angery react",
                }],
            }
            encoded = json.dumps(payload).encode()
            request = Request(
                SLACK_POSTMESSAGE_API,
                data=encoded,
                headers={
                    "Content-type": "application/json",
                    "Authorization": f"Bearer {TOKEN}" },
                method="POST")
            urlopen(request)
        return {}
