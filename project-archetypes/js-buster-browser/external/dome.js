var dome = (function (C) {
    function _assert(pred, msg) {
        if (!pred) { throw new TypeError(msg); }
    }

    function _refute(pred, msg) {
        _assert(!pred, msg);
    }

    function children(elements) {
        if (C.isList(elements)) { return C.flatten(C.map(children, elements)); }
        var results = [], child = elements.firstChild;
        while (child) {
            if (child.nodeType === 1) { results.push(child); }
            child = child.nextSibling;
        }
        return results;
    }

    function id(idStr) {
        return document.getElementById(idStr);
    }

    function byClass(className, parent) {
        var ctx = parent || document;
        if (ctx.getElementsByClassName) {
            return ctx.getElementsByClassName(className);
        }
        var elements = ctx.getElementsByTagName("*"), i, l, result = [];
        var regexp = new RegExp("(^|\\s)" + className + "(\\s|$)");
        for (i = 0, l = elements.length; i < l; ++i) {
            if (regexp.test(elements[i].className)) {
                result.push(elements[i]);
            }
        }
        return result;
    }

    function remove(element) {
        element.parentNode.removeChild(element);
    }

    function hasClassName(className, element) {
        var regexp = new RegExp("(^|\\s)" + className + "(\\s|$)");
        return regexp.test(element.className);
    }

    function addClassName(cn, element) {
        if (C.isList(element)) {
            return C.doall(C.partial(addClassName, cn), element);
        }
        if (hasClassName(cn, element)) { return; }
        element.className = C.trim(element.className + " " + cn);
    }

    function removeClassName(cn, element) {
        if (C.isList(element)) {
            return C.doall(C.partial(removeClassName, cn), element);
        }
        if (!hasClassName(cn, element)) { return; }
        var regexp = new RegExp("(^|\\s)" + cn + "(\\s|$)");
        element.className = C.trim(element.className.replace(regexp, " "));
    }

    // Implementation from jQuery/Sizzle. Simplified.
    function text(elm) {
        _assert(typeof elm !== "undefined" &&
                typeof elm.nodeType === "number",
                "text() expects DOM element");
        var nodeType = elm.nodeType;

        if (nodeType === 1 || nodeType === 9 || nodeType === 11) {
            // Use textContent for elements
            // innerText usage removed for consistency of new lines
            // (see jQuery #11153)
            if (typeof elm.textContent === "string") {
                return elm.textContent;
            }
            var ret = "";
            for (elm = elm.firstChild; elm; elm = elm.nextSibling) {
                ret += text(elm);
            }
            return ret;
        }
        if (nodeType === 3 || nodeType === 4) {
            return elm.nodeValue;
        }
        return "";
    }

    function frag(items) {
        var fragment = document.createDocumentFragment();
        C.doall(C.bind(fragment, "appendChild"), C.toList(items));
        return fragment;
    }

    var el;

    var isContent = function (content) {
        return content !== null && typeof content !== "undefined" &&
            (typeof content.nodeType !== "undefined" ||
             typeof content === "string" ||
             C.isList(content));
    };

    function setData(data, element) {
        var name;
        data = data || {};

        for (name in data) {
            if (data.hasOwnProperty(name)) {
                element.setAttribute("data-" + name, data[name]);
                element["data-" + name] = data[name];
            }
        }
    }

    function getData(property, element) {
        return element.getAttribute("data-" + property);
    }

    var propmap = {
        style: function (element, styles) {
            var property;
            for (property in styles) {
                if (styles.hasOwnProperty(property)) {
                    element.style[property] = styles[property];
                }
            }
        },

        data: function (el, data) {
            setData(data, el);
        }
    };

    function setProp(properties, element) {
        var name, mapper;
        properties = properties || {};

        for (name in properties) {
            if (properties.hasOwnProperty(name)) {
                mapper = propmap[name];
                if (mapper) {
                    mapper(element, properties[name]);
                } else {
                    element[name] = properties[name];
                }
            }
        }
    }

    function append(content, element) {
        _assert(isContent(content),
                "Content should be one or a list of [string, DOM element]");
        content = C.toList(content);
        var i, l;
        for (i = 0, l = content.length; i < l; ++i) {
            if (typeof content[i] === "string") {
                element.appendChild(document.createTextNode(content[i]));
            } else {
                element.appendChild(content[i]);
            }
        }
    }

    function setContent(children, element) {
        _assert(element && typeof element.innerHTML !== "undefined",
                "setContent() needs element");
        element.innerHTML = "";
        append(children, element);
    }

    el = function (tagName, attrProps, content) {
        _refute(arguments.length > 3,
                "Content should be one or a list of [string, DOM element]");
        if (!content && isContent(attrProps)) {
            return el(tagName, {}, attrProps);
        }
        _refute(attrProps && attrProps.tagName,
                "Cannot set attribute property tagName. Use a list when " +
                "adding multiple content elements.");
        var element = document.createElement(tagName);
        setProp(attrProps, element);
        append(content || [], element);
        return element;
    };

    el.toString = function () {
        return "dome.el()";
    };

    C.doall(function (tagName) { el[tagName] = C.partial(el, tagName); }, [
        "a", "br", "div", "fieldset", "form", "h2", "h3", "h4",
        "h5", "img", "input", "label", "li", "p", "span", "strong",
        "textarea", "ul", "span", "select", "option", "ol", "iframe",
        "table", "tr", "td", "pre", "button", "i"
    ]);

    /** docs:function-list */
    return {
        propmap: propmap,
        el: el,
        setProp: setProp,
        append: append,
        setContent: setContent,
        children: children,
        id: id,
        byClass: byClass,
        remove: remove,
        frag: frag,
        text: text,
        data: { get: getData, set: setData },
        cn: { has: hasClassName, add: addClassName, rm: removeClassName }
    };
}(this.cull));

/*global cull, dome, window*/

// This is a modified version of code by Juriy Zaytsev originally published at
// http://msdn.microsoft.com/en-us/magazine/ff728624.aspx
(function (C, D) {
    function isHostMethod(object, method) {
        return (/^(?:function|object|unknown)$/).test(typeof object[method]);
    }

    var getUniqueId = (function () {
        if (typeof document.documentElement.uniqueID !== "undefined") {
            return function (element) {
                return element.uniqueID;
            };
        }
        var uid = 0;
        return function (element) {
            if (!element.__uniqueID) {
                element.__uniqueID = "uniqueID__" + uid;
                uid += 1;
            }
            return element.__uniqueID;
        };
    }());

    var elements = {}, on, off, d = document.documentElement;

    function createWrappedHandler(uid, handler) {
        return function (e) {
            handler.call(elements[uid], e || window.event);
        };
    }

    function createListener(uid, handler) {
        return {
            handler: handler,
            wrappedHandler: createWrappedHandler(uid, handler)
        };
    }

    if (isHostMethod(d, "addEventListener") &&
            isHostMethod(d, "removeEventListener") &&
            isHostMethod(window, "addEventListener") &&
            isHostMethod(window, "removeEventListener")) {
        on = function (element, eventName, handler) {
            element.addEventListener(eventName, handler, false);
            return {
                cancel: function () { off(element, eventName, handler); }
            };
        };

        off = function (element, eventName, handler) {
            element.removeEventListener(eventName, handler, false);
        };
    } else if (isHostMethod(d, "attachEvent") &&
                   isHostMethod(d, "detachEvent") &&
                   isHostMethod(window, "attachEvent") &&
                   isHostMethod("detachEvent")) {
        var listeners = {};

        on = function (element, eName, handler) {
            var uid = getUniqueId(element);
            elements[uid] = element;
            if (!listeners[uid]) { listeners[uid] = {}; }
            if (!listeners[uid][eName]) { listeners[uid][eName] = []; }
            var listener = createListener(uid, handler);
            listeners[uid][eName].push(listener);
            element.attachEvent("on" + eName, listener.wrappedHandler);

            return { cancel: function () { off(element, eName, handler); } };
        };

        off = function (element, eName, handler) {
            var uid = getUniqueId(element);
            if (!listeners[uid] || !listeners[uid][eName]) { return; }
            listeners[uid][eName] = C.select(function (listener) {
                if (listener.handler !== handler) { return true; }
                element.detachEvent("on" + eName, listener.wrappedHandler);
            }, listeners[uid][eName]);
        };
    }

    function delegate(delegator, element, event, handler) {
        on(element, event, function (e) {
            if (delegator(e.target, event, e)) {
                handler.call(e.target, e);
            }
        });
    }

    delegate.bycn = function (className, element, event, handler) {
        delegate(C.partial(D.cn.has, className), element, event, handler);
    };

    dome.on = on;
    dome.off = off;
    dome.delegate = delegate;

    dome.propmap.events = function (el, events) {
        C.doall(function (prop) {
            on(el, prop, events[prop]);
        }, C.keys(events));
    };
}(cull, dome));