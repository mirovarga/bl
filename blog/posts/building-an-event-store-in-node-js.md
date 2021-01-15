---
title: Building an Event Store in Node.js
description: As I quite like the idea of event sourcing I decided to build a
  simple event store in Node.js.
created: 2015-01-21T00:00:00Z
tags: [JavaScript]
---

As I quite like the idea of
[event sourcing](http://docs.geteventstore.com/introduction/event-sourcing-basics)
I decided to build a simple event store in Node.js.

We'll be able to store events, retrieve events and react (do something useful)
when an event has been stored.

> You can find the complete code in the GitHub [repository](https://github.com/mirovarga/eventstore-node).

## Adding Events

Each event is associated with a stream. A stream can be thought of as a container
for events related to a concrete object, for example a customer.

For simplicity, we'll store all events in memory, but it should be quite easy to
keep them in a file or a database.

```javascript
var events = {};

exports.add = function (stream, event, payload) {
  var e = {
    event: event,
    payload: payload,
    timestamp: Date.now()
  };

  storeEvent(stream, e);
};

function storeEvent(stream, event) {
  events[stream] = events[stream] || [];
  events[stream].push(event);
}
```

## Retrieving Events

We can retrieve all events from a stream easily.

```javascript
exports.all = function (stream) {
  return events[stream] || [];
};
```

## Reacting to Events

To do something useful after an event has been stored, we first have to register
a function that will handle the event.

> Handlers are often used as projections in
  a [CQRS](http://martinfowler.com/bliki/CQRS.html) system to update a query model.

```javascript
var handlers = {};

exports.on = function (event, handler) {
  handlers[event] = handlers[event] || [];
  handlers[event].push(handler);
};
```

Next we'll have to modify the `add` function to also call all registered handlers.

```javascript
exports.add = function (stream, event, payload) {
  // ...
  handleEvent(e);
};

function handleEvent(event) {
  var hs = handlers[event.event] || [];
  for (var i = 0; i < hs.length; i++)
    hs[i](event);
}
```

> When a handler is actually called the stored event is passed as its only
  argument.

## Testing

Here is a quick [Mocha](http://mochajs.org) test. It should be stored under the
`test` directory.

```javascript
var es = require('../eventstore.js');
var assert = require('assert');

describe('eventstore', function () {
  describe('#add', function () {
    it('should add an event to the store', function (done) {
      es.add('customer1', 'created', {
        firstName: 'John',
        lastName: 'Smith'
      });

      assert.equal(1, es.all('customer1').length);
      assert.equal(0, es.all('customer2').length);
      done();
    });
  });

  describe('#on', function () {
    it('should react after an event has been stored', function (done) {
      es.on('created', function (event) {
        done();
      });
      es.on('deleted', function (event) {
        throw new Error("This shouldn't happen");
      });

      es.add('customer1', 'created', {
        firstName: 'John',
        lastName: 'Smith'
      });
    });
  });
});
```
