import { Notifier } from "./notifier.js";

export function isRenderable(obj) {
  if (typeof obj === "string") return true;
  if (typeof obj === "function") return true;
  if (typeof obj !== "object") return false;
  if (Array.isArray(obj)) return true;
  if (obj instanceof Node) return true;
  return false;
}

export function e(tagName, props, ...children) {
  let tag = document.createElement(tagName);
  if (isRenderable(props)) {
    children.unshift(props);
  } else if (props) {
    if ('attributes' in props) {
      for (const key in props.attributes) {
        if (props.attributes.hasOwnProperty(key)) {
          tag.setAttributes(key, props.attributes[key]);
        }
      }
      delete props.attributes;
    }
    if ('class' in props) {
      props.className = Array.isArray(props.class) ? props.class.join(' ') : props.class;
      delete props.class;
    }
    if ('style' in props) {
      Object.assign(tag.style, props.style)
      delete props.style;
    }
    Object.assign(tag, props);
  }

  render(children, tag);
  return tag;
}

export function render(obj, tag, insertBefore = null) {
  if (typeof obj === "string") {
    tag.insertBefore(document.createTextNode(obj), insertBefore);
  } else if (Array.isArray(obj)) {
    obj.forEach(r => render(r, tag, insertBefore))
  } else if (typeof obj === "function") {
    render(obj(tag), tag, insertBefore);
  } else if (obj !== undefined) {
    tag.insertBefore(obj, insertBefore);
  }
}

function makeMarker() {
  return document.createComment("");
}

/**
 * 
 * @param {Notifier} update 
 * @param {Function} render 
 */
export function bind(update, func) {
  return node => {
    const start = makeMarker();
    const end = makeMarker();
    const val = func(update.value);

    update.subscribe((value) => {
      const val = func(value);
      while (start.nextSibling !== null && start.nextSibling !== end) start.nextSibling.remove();
      render(val, node, start.nextSibling);
    });

    return [start, val, end];
  }
}

export function async(loader, promise) {
  return node => {
    const start = makeMarker();
    const end = makeMarker();

    if (typeof promise === "function") promise = promise();

    promise.then(data => {
      while (start.nextSibling !== null && start.nextSibling !== end) start.nextSibling.remove();
      render(data, node, start.nextSibling);
      end.remove();
      start.remove();
    });

    return [start, loader, end];
  }
}
