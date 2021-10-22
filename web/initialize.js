function initialize(path) {
    let element = document.createElement("script");
    element.type = "text/javascript";
    element.src = path + "ogre.tools.js";
    document.head.appendChild(element);
}

function main() {
    if (window.location.hostname === "localhost") {
        initialize("assets/");
        return;
    }

    return;
}

document.addEventListener("DOMContentLoaded", main);
