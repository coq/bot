FROM coqbot

COPY coqbot-config.toml ./

CMD ["./bot.exe", "coqbot-config.toml"]
