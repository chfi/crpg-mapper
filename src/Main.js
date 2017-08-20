"use strict";

exports.inputEvent = function(el) {
    return function(sub) {
        el.addEventListener('input', function(v) {
            sub(v.target.value);
        });
    };
};

exports.buttonEvent = function(el) {
    console.log("adding event listener");
    return function(sub) {
        el.addEventListener('click', function(ev) {
            console.log(ev);
            sub({});
        });
    };
};

exports.getElement = function(id) {
    return function() {
        return document.getElementById(id);
    };
};

exports.getButton = function(id) {
    return function() {
        return document.getElementById(id);
    };
};
