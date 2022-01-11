import { chars } from '../utils';
import { Site } from './types';

const gmail: Site = {
  domain: 'mail.google.com',
  onLoad: () => api.unmapAllExcept(chars`f`, /mail\.google\.com/),
};

export default gmail;
