self.addEventListener('install', e => {
    let timeStamp = Date.now();
    e.waitUntil(
        caches.open('zenoss').then(cache => {
            return cache.addAll([
                './',
                './index.html',
            ])
        })
    )
})

self.addEventListener('fetch', function(event) {
    console.log(event.request.url)
    event.respondWith(
        caches.match(event.request)
        .then(function(response) {
            return response || fetch(event.request)
        })
    )
})