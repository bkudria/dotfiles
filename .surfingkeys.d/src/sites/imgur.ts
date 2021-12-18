import { Site } from './types';

const imgur: Site = {
  domain: 'imgur.com',
  onLoad: () => {
    mapkey(
      '!auxRight',
      'Imgur Next Item',
      () =>
        (document?.querySelector(
          `a.Navigation-next`
        ) as HTMLElement).click(),
      { domain: /imgur\.com/ }
    );
  },
};

export default imgur;
