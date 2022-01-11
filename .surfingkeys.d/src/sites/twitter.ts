import { Site } from './types';

const twitter: Site = {
  domain: 'twitter.com',
  onLoad: () => {
    api.mapkey(
      ',n',
      'Nitter',
      () =>
        (window.location.href = window.location.href.replace(
          'twitter.com',
          'nitter.net'
        )),
      { domain: /twitter\.com/ }
    );
  },
};

export default twitter;
