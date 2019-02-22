#!/usr/bin/python3
import os
import sys
import json
from random import choice
from urllib.request import urlopen, Request
import hmac
import hashlib
from time import time
import boto3


SLACK_OAUTH_API = "https://slack.com/api/oauth.access"
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

SDB_DOMAIN = "BOT_TOKENS"


def verify_request_comes_from_slack(event):
    try:
        SIGNING_SECRET = os.environ["SLACK_SIGNING_SECRET"].encode()
    except KeyError:
        raise RuntimeError("SLACK_SIGNING_SECRET environment variable is not set.")

    headers = event.get("headers") or {}
    if SLACK_TIMESTAMP not in headers or SLACK_SIGNATURE not in headers:
        raise RuntimeError("Missing Slack verification headers")

    timestamp = headers[SLACK_TIMESTAMP]
    if time() - int(timestamp) > 60:
        raise RuntimeError("Slack message too old - possible replay")

    raw_body = event["body"]
    signature_base = f"v0:{timestamp}:{raw_body}".encode()
    signature = (
        "v0=" + hmac.new(SIGNING_SECRET, signature_base, hashlib.sha256).hexdigest()
    )
    if signature != headers[SLACK_SIGNATURE]:
        raise RuntimeError("Slack signature not matching")


def put_token(sdb, domain, team_id, token):
    response = sdb.put_attributes(
        DomainName=domain,
        ItemName=team_id,
        Attributes=[{"Name": "token", "Value": token, "Replace": True}],
    )


def get_token(sdb, domain, team_id):
    response = sdb.get_attributes(
        DomainName=domain, ItemName=team_id, AttributeNames=["token"]
    )
    return response.get("Attributes", [{}])[0].get("Value", None)


def register(params):
    try:
        CLIENT_ID = os.environ["CLIENT_ID"]
        CLIENT_SECRET = os.environ["CLIENT_SECRET"]
    except KeyError:
        raise RuntimeError("Slack client id or secret environment variable is not set.")

    code = params.get("code")
    if not code:
        raise RuntimeError("Malformed register request")

    data = f"client_id={CLIENT_ID}&client_secret={CLIENT_SECRET}&code={code}"
    request = Request(
        SLACK_OAUTH_API,
        method="POST",
        data=data.encode(),
        headers={"Content-type": "application/x-www-form-urlencoded"},
    )
    with urlopen(request) as response:
        auth_details = json.load(response)

    team_id = auth_details.get("team_id")
    bot_token = auth_details.get("bot", {}).get("bot_access_token")
    if not auth_details.get("ok") or not team_id or not bot_token:
        raise RuntimeError("Error getting auth tokens")

    sdb = boto3.client("sdb", region_name="eu-west-1")
    put_token(sdb, SDB_DOMAIN, team_id, bot_token)


def lambda_handler(event, context):
    print(event)
    if event.get("resource") == "/AngeryBot/register":
        register(event.get("queryStringParameters", {}))
        return {
            "body": "We will now deliver your angeries - don't forget to invite the Angery bot to your channels!"
        }

    verify_request_comes_from_slack(event)

    body = json.loads(event["body"])
    print(body)

    if "challenge" in body:
        return {"body": body["challenge"]}

    else:
        team_id = body.get("team_id")
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

            sdb = boto3.client("sdb", region_name="eu-west-1")
            token = get_token(sdb, SDB_DOMAIN, team_id)
            payload = {
                "channel": channel,
                "attachments": [{"image_url": img, "fallback": "Angery react"}],
            }
            encoded = json.dumps(payload).encode()
            request = Request(
                SLACK_POSTMESSAGE_API,
                data=encoded,
                headers={
                    "Content-type": "application/json",
                    "Authorization": f"Bearer {token}",
                },
                method="POST",
            )
            print(encoded)
            urlopen(request)
        return {}
