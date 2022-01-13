import { Site } from './types';

const netflix: Site = {
  domain: 'www.netflix.com',
  onLoad: () => api.unmap('<Space>'),
};

export default netflix;
