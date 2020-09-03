type Selector = string;

export const chars = ([chars]: TemplateStringsArray) => chars.split('');
export const words = ([words]: TemplateStringsArray) => words.split(' ');

export const removeSticky = () => {
  let position;
  for (let element of Array.from(document.querySelectorAll('body *'))) {
    position = getComputedStyle(element).position;
    if (position === 'fixed' || position === 'sticky') {
      element.parentNode?.removeChild(element);
    }
  }
};

const getElement = (selector: Selector) => getElements(selector)[0];

const dispatchMouseClicks = (elements: HTMLElement[]) =>
  elements.forEach(element => Hints.dispatchMouseClick(element));

export const openStoryAndComments = ({
  story,
  link,
  comments,
}: {
  story: Selector;
  link: Selector;
  comments: Selector;
}) => {
  Hints.create(
    story,
    (storyElement: HTMLElement) => {
      dispatchMouseClicks([
        getElement(`*[id="${storyElement.id}"] ${link}`),
        getElement(`*[id="${storyElement.id}"] ${comments}`),
      ]);
    },
    { multipleHits: true }
  );
};

export const createSuggestionItem = (html: string, props = {}) => {
  const li = document.createElement('li');
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};
