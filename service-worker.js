const cacheName = 'Elm-Cards';
const dataCacheName = 'Elm-Cards-Data-v1';
const dataUrl = 'https://glosbe.com/gapi/translate';

const filesToCache = [
  // '/',
  // '/index.html',
  // '/elm.js',
  // '/app.js',
  'https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css',
  'https://use.fontawesome.com/releases/v5.1.0/js/all.js',
];

self.addEventListener('install', e => {
  console.log('[ServiceWorker] Install');
  e.waitUntil(
    caches.open(cacheName).then(cache => {
      console.log('[ServiceWorker] Caching app shell');
      return cache.addAll(filesToCache);
    }),
  );
});

self.addEventListener('activate', e => {
  console.log('[ServiceWorker] Activate');
  e.waitUntil(
    caches.keys().then(keyList =>
      Promise.all(
        keyList.map(key => {
          if (key !== cacheName && key !== dataCacheName) {
            console.log('[ServiceWorker] Removing old cache', key);
            return caches.delete(key);
          }
        }),
      ),
    ),
  );
  return self.clients.claim();
});

self.addEventListener('fetch', e => {
  const url = e.request.url;

  console.log('[Service Worker] Fetch', url);
  if (url.indexOf(dataUrl) > -1) {
    e.respondWith(
      caches.match(url).then(response => {
        console.log('[Service Worker] in cache:', response);
        if (response) {
          console.log('[Service Worker] using cached!');
          return response;
        } else {
          console.log('[Service Worker] Fetching from network');
          return caches.open(dataCacheName).then(cache => {
            return fetch(e.request).then(response => {
              console.log('[Service Worker] Storing to cache');
              return cache.put(url, response.clone()).then(() => response);
            });
          });
        }
      }),
    );
  } else {
    e.respondWith(caches.match(e.request).then(response => response || fetch(e.request)));
  }
});
