FROM coqbot

COPY coqbot.toml ./

CMD ["./bot.exe", "coqbot.toml"]
