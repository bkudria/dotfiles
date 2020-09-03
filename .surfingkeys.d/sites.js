const wpDefaultIcon = `
data:image/svg+xml,%3C%3Fxml%20version%3D%221.0%22%20encoding%3D%22UTF-8%22%3F%3E%0A%3Csvg%20en
able-background%3D%22new%200%200%2056%2056%22%20viewBox%3D%220%200%2056%2056
%22%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%3E%0A%3Cpath%20d%3D
%22m0%200h56v56h-56z%22%20fill%3D%22%23eee%22%2F%3E%0A%3Cpath%20d%3D%22M36.4
%2013.5h-18.6v24.9c0%201.4.9%202.3%202.3%202.3h18.7v-25c.1-1.4-1-2.2-2.4-2.2
zm-6.2%203.5h5.1v6.4h-5.1v-6.4zm-8.8%200h6v1.8h-6v-1.8zm0%204.6h6v1.8h-6v-1.
8zm0%2015.5v-1.8h13.8v1.8h-13.8zm13.8-4.5h-13.8v-1.8h13.8v1.8zm0-4.7h-13.8v-
1.8h13.8v1.8z%22%20fill%3D%22%23999%22%2F%3E%0A%3C%2Fsvg%3E`.replace(
  /\s+/g,
  ""
);

mapkey(
  "\\tts",
  "Turn Touch South [Roll20]",
  () =>
    document
      .querySelector(`*[id="player_-M2V74cjR6ybxSEet2kQ"] .mute-audio`)
      .click(),
  { domain: /roll20\.net/ }
);

map("\\tte", "]]", /imgur\.com/, "Turn Touch East [imgur]");

unmapAllExcept(chars`xhlf`, /mail\.google\.com/);
unmap("`", /roll20.net/);

const getElement = (selector) => getElements(selector)[0];

const dispatchMouseClicks = (elements) =>
  elements.forEach((element) => Hints.dispatchMouseClick(element));

const openStoryAndComments = ({ story, link, comments }) => {
  Hints.create(
    story,
    (storyElement) => {
      dispatchMouseClicks([
        getElement(`*[id="${storyElement.id}"] ${link}`),
        getElement(`*[id="${storyElement.id}"] ${comments}`),
      ]);
    },
    { multipleHits: true }
  );
};

const createSuggestionItem = (html, props = {}) => {
  const li = document.createElement("li");
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};

const wikipediaCallback = (response) =>
  Object.values(JSON.parse(response.text).query.pages).map((p) => {
    const img = p.thumbnail ? p.thumbnail.source : wpDefaultIcon;
    const desc = p.description ? p.description : "";
    return createSuggestionItem(
      `<div style="padding:5px;display:grid;grid-template-columns:60px 1fr;grid-gap:15px">
              <img style="width:60px" src="${img}" alt="${p.title}">
              <div>
                <div class="title"><strong>${p.title}</strong></div>
                <div class="title">${desc}</div>
              </div>
            </div>`,
      { url: p.fullurl }
    );
  });

sites = {
  "amazon.com": {
    engines: [
      {
        name: "Amazon",
        alias: "az",
        search: "https://smile.amazon.com/s/?field-keywords=",
        completion:
          "https://completion.amazon.com/search/complete?method=completion&mkt=1&search-alias=aps&q=",
        callback: (response) => JSON.parse(response.text)[1],
      },
    ],
  },

  "duckduckgo.com": {
    engines: [
      {
        name: "DuckDuckGo",
        alias: "ddg",
        single: "d",
        search: "https://duckduckgo.com/?q=",
        completion: "https://duckduckgo.com/ac/?q=",
        callback: (response) => JSON.parse(response.text).map((r) => r.phrase),
      },
      {
        name: "DuckDuckGo!",
        alias: "ddl",
        single: "l",
        search: "https://duckduckgo.com/?q=\\",
        completion: "https://duckduckgo.com/ac/?q=\\",
        callback: (response) => JSON.parse(response.text).map((r) => r.phrase),
      },
    ],
  },
  "google.com": {
    engines: [
      {
        name: "Google",
        alias: "go",
        search: "https://www.google.com/search?q=",
        completion:
          "https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=",
        callback: (response) => JSON.parse(response.text)[1],
      },
    ],
  },
  "lobste.rs": {
    mappings: [
      {
        path: /^(page.*)?$/,
        keys: "cf",
        description: "#1Open Link and Comments [Lobsters]",
        fn: () =>
          openStoryAndComments({
            story: "li.story",
            link: "a.u-url",
            comments: ".comments_label a",
          }),
      },
    ],
  },
  "news.ycombinator.com": {
    mappings: [
      {
        path: /^$/,
        keys: "cf",
        description: "#1Open Link and Comments [Lobsters]",
        fn: () =>
          openStoryAndComments({
            story: "tr.athing",
            link: "a.storylink",
            comments: "+ tr td.subtext > a:last-of-type",
          }),
      },
    ],
  },
  "wikipedia.org": {
    engines: [
      {
        name: "Wikipedia",
        alias: "wiki",
        single: "w",
        search: "https://en.wikipedia.org/w/index.php?search=",

        completion: `
          https://en.wikipedia.org/w/api.php?action=query&format=json
          &generator=prefixsearch&prop=info|pageprops%7Cpageimages%7Cdescription
          &redirects=&ppprop=displaytitle&piprop=thumbnail&pithumbsize=100&pilimit=6
          &inprop=url&gpssearch=`.replace(/\s+/g, ""),

        callback: wikipediaCallback,
      },
    ],
  },
};

chars`bdghwyse`.forEach((searchAlias) => {
  removeSearchAliasX(searchAlias, "s");
  removeSearchAliasX(searchAlias, "o");
});

Object.entries(sites).forEach(([domain, config]) => {
  (config.engines || []).forEach((engine) => {
    addSearchAliasX(
      engine.alias,
      engine.name,
      engine.search,
      "s",
      engine.completion,
      engine.callback
    );

    mapkey(`o${engine.single || engine.alias}`, `#8Search ${engine.name}`, () =>
      Front.openOmnibar({ type: "SearchEngine", extra: engine.alias })
    );
  });

  if (domain == window.location.hostname) {
    (config.mappings || []).forEach((mapping) => {
      if (window.location.pathname.slice(1).match(mapping.path)) {
        mapkey(mapping.keys, mapping.description, mapping.fn);
      }
    });

    config.onLoad && config.onLoad();
  }
});
