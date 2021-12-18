import { Site } from './types';

const roll20: Site = {
  domain: 'app.roll20.net',
  onLoad: () => {
    unmap('`', /roll20.net/);
    mapkey(
      '!auxDown',
      'Roll20 Mute Toggle',
      () =>
        (document?.querySelector(
          `*[id="player_-M2V74cjR6ybxSEet2kQ"] .mute-audio`
        ) as HTMLElement).click(),
      { domain: /roll20\.net/ }
    );
  },
};

export default roll20;
