class Clipper
  new: (maxEntries = 100, maxAge = 1000) =>
    @dbPath = "#{hs.configdir}/.clips.sqlite"
    @maxEntries = maxEntries
    @maxAge = maxAge
    @db = hs.sqlite3.open(@dbPath)

  start: =>
    @dbInit!
    @clipWatcher = hs.pasteboard.watcher.new((clipped) -> @recordNewClip(clipped))

  stop: =>
    @db\close! if @db

  privateClip: () ->
    skipTypes = {
      "org.nspasteboard.TransientType"
      "org.nspasteboard.ConcealedType"
      "org.nspasteboard.AutoGeneratedType"
    }
    hs.fnutils.some(
      hs.pasteboard.contentTypes!,
      (type) -> hs.fnutils.contains(skipTypes, type)
    )

  recordNewClip: (clip) =>
    return if @privateClip!
    return if #clip < 5
    hs.alert(clip, 0.2)

    clip = clip\gsub("^%s*(.-)%s*$", "%1")
    sql = "insert into clips(clip, ts) values (:clip, :time) on conflict(clip) do update set ts=:time"
    @dbExec(sql, {clip: clip, time: os.time!})

  search: (query) =>
    query = query\gsub('[^a-zA-Z0-9 ]', '')
    if string.len(query) > 1
      @dbExec("select clip, ts from clip_fts where clip match :q limit 10", q: "#{query}*")
    else
      @dbExec("select clip, ts from clip_fts order by ts desc limit 20")

  dbExec: (sql, names = {}) =>
    query = @db\prepare(sql)
    query\bind_names(names)
    result = {}
    for r in query\nrows!
      table.insert(result, r)
    query\reset!
    result

  dbInit: =>
    @dbExec("create table if not exists clips(
      id integer not null primary key autoincrement,
      clip text not null unique,
      ts integer
    )")

    @dbExec("create virtual table if not exists clip_fts using fts5(
      clip,
      ts,
      content=clips,
      content_rowid=id
    )")

    @dbExec("create trigger if not exists clip_after_insert after insert on clips begin
      insert into clip_fts(rowid, clip, ts) values (new.id, new.clip, new.ts);
    end")

    @dbExec("create trigger if not exists clip_after_delete after delete on clips begin
      insert into clip_fts(clip_fts, rowid, clip, ts) values ('delete', old.id, old.clip, old.ts);
    end")

    @dbExec("create trigger if not exists clip_after_update after update on clips begin
      insert into clip_fts(clip_fts, rowid, clip, ts) values ('delete', old.id, old.clip, old.ts);
      insert into clip_fts(rowid, clip, ts) values (new.id, new.clip, new.ts);
    end")
