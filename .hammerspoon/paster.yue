class Paster
  new: (clipper, typer) =>
    @clipper = clipper
    @typer = typer
    @chooser = hs.chooser.new((item) -> @paste(item))
    @chooser\queryChangedCallback(() -> @setChoices())

  paste: (item) => @typer\type(item.text) if item

  setChoices: () =>
    query = @chooser\query!
    results = @clipper\search(query)
    choices = hs.fnutils.imap(results, (item) ->
      { text: item.clip, subText: os.date("%b %d, %H:%M:%S", item.ts) })
    if #query > 1 and #results > 1
      choices = hs.fnutils.concat(
        choices, {{
          text: table.concat(hs.fnutils.imap(results, (result) -> result.clip), "\n"),
            subText: "Paste all #{#results} items"
        }}
      )
    @chooser\choices(choices)

  select: () =>
    @chooser\query(nil)
    @chooser\show!
