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

mapkey(
  "cf",
  "#1Open Link and Comments [Lobsters]",
  () =>
    openStoryAndComments({
      story: "li.story",
      link: "a.u-url",
      comments: ".comments_label a",
    }),
  { domain: /lobste\.rs/ }
);

mapkey(
  "cf",
  "#1Open Link and Comments [HN]",
  () =>
    openStoryAndComments({
      story: "tr.athing",
      link: "a.storylink",
      comments: "+ tr td.subtext > a:last-of-type",
    }),
  { domain: /news\.ycombinator\.com/ }
);
