class Paster
  new: (clipper, typer) =>
    @clipper = clipper
    @typer = typer
    @chooser = hs.chooser.new((item) -> @paste(item))
    @chooser\queryChangedCallback(() -> @setChoices())

  paste: (item) => @typer\type(item.text) if item

  setChoices: () =>
    query = @chooser\query!
    choices = hs.fnutils.imap(@clipper\search(query), (item) ->
      { text: item.clip, subText: os.date("%b %d, %H:%M:%S", item.ts) })
    @chooser\choices(choices)

  select: () =>
    @chooser\query(nil)
    @chooser\show!
