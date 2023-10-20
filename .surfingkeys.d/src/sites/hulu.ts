import { Site } from './types';

const hulu: Site = {
  domain: 'www.hulu.com',
  onLoad: () => api.unmap('<Space>'),
};

export default hulu;
