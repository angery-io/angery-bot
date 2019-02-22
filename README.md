# Angery Bot for Slack
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/ambv/black)
[![Add to Slack](https://platform.slack-edge.com/img/add_to_slack.png)](https://slack.com/oauth/authorize?client_id=428856005156.558410002486&scope=bot,commands)

Do you want more angeries? Angery Bot for Slack delivers extra angery to your workspace.

## Demo
[![gif with examples][example-gif]][example-gif]

## Instrustions for deployment

- Deploy the files to AWS Lambda
- Setup a new Slack app, add a bot user
- Take a note of *Bot User OAuth Access Token*, set the `SLACK_TOKEN` environment variable in the AWS
- Setup a AWS API Gateway, add an event scription on Slack with the API
- Subscribe to `message.channels`, `message.im` and `reaction_added` events
- Invite your bot to the channel
- Enjoy your angeries

Note: You can add extra angeries to `resources/angery.txt`

## Future work

- Publish this as a Slack app so our users don't have to set it up themselves
- Customisations


[example-gif]: https://raw.githubusercontent.com/angery-io/angery-bot/master/demo.gif
