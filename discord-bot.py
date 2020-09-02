import os
import discord
from random import choice

try:
    TOKEN = os.environ["TOKEN"]
except KeyError:
    raise RuntimeError("TOKEN not set")


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
with open(ANGERY_RESOURCES, "r") as f:
    IMGS = [line.strip() for line in f.readlines()]

client = discord.Client(token=TOKEN)


@client.event
async def on_message(message):
    if message.author == client.user:
        return
    search_space = message.content.lower()
    if not (
        search_space
        and any(trigger in search_space for trigger in ANGERY_TRIGGER)
    ):
        return
    if any(suppressor in search_space for suppressor in ANGERY_SUPPRESSOR):
        await message.add_reaction("😡")
        return

    embed = discord.Embed()
    embed.set_image(url=choice(IMGS))
    await message.channel.send(embed=embed)


client.run(TOKEN)
