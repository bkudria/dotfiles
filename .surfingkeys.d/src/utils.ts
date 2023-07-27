type Selector = string;

export const chars = ([chars]: TemplateStringsArray) => chars.split('');
export const words = ([words]: TemplateStringsArray) => words.split(' ');

export const removeSticky = ({
  root,
  nice,
}: {
  root: Document | DocumentFragment;
  nice: boolean;
}) => {
  for (let element of Array.from(root.querySelectorAll('*'))) {
    if (element.shadowRoot) {
      removeSticky({ root: element.shadowRoot, nice: nice });
    }

    if (element instanceof HTMLElement) {
      if (isFixedOrSticky(element) && element.className !== 'sk_ui') {
        if (nice) {
          if (isHorizontal(element)) {
            if (isFixed(element)) {
              element.style.position = 'absolute';
            }
            if (isSticky(element)) {
              element.style.position = 'relative';
            }
            if (element.style.top !== '') {
              element.style.top = '0';
            }
            console.log('unstickied', element, element.style.position);
          }
        } else {
          element.parentNode?.removeChild(element);
          console.log('removed', element);
        }
      }
    }
  }
};

const isHorizontal = (element: HTMLElement) => {
  let rect = element.getBoundingClientRect();
  return rect.width / rect.height > 2;
};

const isFixed = (element: HTMLElement) => {
  let style = getComputedStyle(element);
  return style.position === 'fixed';
};

const isSticky = (element: HTMLElement) => {
  let style = getComputedStyle(element);
  return style.position === 'sticky';
};

const isFixedOrSticky = (element: HTMLElement) =>
  isFixed(element) || isSticky(element);

const getElement = (selector: Selector) =>
  api.getClickableElements(selector)[0];

const dispatchMouseClicks = (elements: HTMLAnchorElement[]) =>
  new Set(elements.map(element => element.href)).forEach(href =>
    api.RUNTIME('openLink', {
      tab: {
        tabbed: true,
        active: false,
      },
      url: href,
    })
  );

export const openStoryAndComments = ({
  story,
  link,
  comments,
}: {
  story: Selector;
  link: Selector;
  comments: Selector;
}) => {
  api.Hints.create(story, (storyElement: HTMLElement) => {
    dispatchMouseClicks([
      getElement(`*[id="${storyElement.id}"] ${link}`) as HTMLAnchorElement,
      getElement(`*[id="${storyElement.id}"] ${comments}`) as HTMLAnchorElement,
    ]);
  });
};

export const createSuggestionItem = (html: string, props = {}) => {
  const li = document.createElement('li');
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};

export const scrollMostPage = () =>
  document.scrollingElement?.scrollBy({
    behavior: 'smooth',
    left: 0,
    top: 0.9 * window.innerHeight,
  });

export const darkReaderEnabled = () =>
  document.querySelector('style.darkreader') ||
  document.querySelector('style#dark-reader-style');
