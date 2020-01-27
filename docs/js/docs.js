/**
 * Toggle an specific class to the received DOM element.
 * @param {string}	elemSelector The query selector specifying the target element.
 * @param {string}	[activeClass='active'] The class to be applied/removed.
 */
function toggleClass(elemSelector, activeClass = 'active') {
	const elem = document.querySelector(elemSelector);
  if (elem) {
    elem.classList.toggle(activeClass);
  }
}

/**
 * Toggle specific classes to an array of corresponding DOM elements.
 * @param {Array<string>}	elemSelectors The query selectors specifying the target elements.
 * @param {Array<string>}	activeClasses The classes to be applied/removed.
 */
function toggleClasses(elemSelectors, activeClasses) {
  elemSelectors.map((elemSelector, idx) => {
		toggleClass(elemSelector, activeClasses[idx]);
	});
}

/**
 * Remove active class from siblings DOM elements and apply it to event target.
 * @param {Element}		element The element receiving the class, and whose siblings will lose it.
 * @param {string}		[activeClass='active'] The class to be applied.
 */
function activate(element, activeClass = 'active') {
	[...element.parentNode.children].map((elem) => elem.classList.remove(activeClass));
	element.classList.add(activeClass);
}

/**
 * Remove active class from siblings parent DOM elements and apply it to element target parent.
 * @param {Element}		element The element receiving the class, and whose siblings will lose it.
 * @param {string}		[activeClass='active'] The class to be applied.
 */
function activateParent(element, activeClass = 'active') {
	const elemParent = element.parentNode;
	activate(elemParent, activeClass);
}

/**
 * Remove active class from siblings parent DOM elements and apply it to element target parent.
 * @param {Element}		element The element receiving the class, and whose siblings will lose it.
 * @param {string}		[activeClass='active'] The class to be applied.
 */
function toggleParent(element, activeClass = "active") {
  const elemParent = element.parentNode;
  if (elemParent) {
    elemParent.classList.toggle(activeClass);
  }
}

/**
 * This will make the specified elements click event to show/hide the menu sidebar.
 */
function activateToggle() {
  const menuToggles = document.querySelectorAll("#menu-toggle, #main-toggle");
  if (menuToggles) {
    [...menuToggles].map(elem => {
      elem.onclick = e => {
        e.preventDefault();
				toggleClass("#site-sidebar", "toggled");
        toggleClass("#site-doc", "expanded");
      };
    });
  }
}

/**
 * This will make the specified elements click event to behave as a menu
 * parent entry, or a link, or sometimes both, depending on the context.
 */
function activateMenuNesting() {
  const menuParents = document.querySelectorAll(".drop-nested");
  if (menuParents) {
    [...menuParents].map(elem => {
      elem.onclick = e => {
        e.preventDefault();
        toggleParent(elem, "open");
        const elementType = e.currentTarget.tagName.toLowerCase();
        if (elementType === "a") {
          const linkElement = e.currentTarget;
          const linkElementParent = linkElement.parentNode;
          const destination = linkElement.href;
          if (
            destination !== window.location.href &&
            !linkElementParent.classList.contains("active")
          ) {
            window.location.href = destination;
          }
        }
      };
    });
  }
}

/**
 * Aux function to retrieve repository stars and watchers count info from
 * GitHub API and set it on its proper nodes.
 */
async function loadGitHubStats() {
  const ghInfo = document.querySelector('meta[property="github-info"]');
  const ghOwner = ghInfo.dataset.githubOwner;
  const ghRepo = ghInfo.dataset.githubRepo;

  if (ghOwner && ghRepo) {
    const ghAPI = `https://api.github.com/repos/${ghOwner}/${ghRepo}`;
    const ghDataResponse = await fetch(ghAPI);
    const ghData = await ghDataResponse.json();
		const ghStars = ghData.stargazers_count;
		const starsElement = document.querySelector("#stars-count");
		if (starsElement) {
			if (ghStars) {
				starsElement.textContent = `☆ ${ghStars}`;
			}
			else {
				starsElement.remove();
			}
		}
  }
}

/**
 * Function to create an anchor with an specific id
 * @param {string}    id The corresponding id from which the href will be created.
 * @returns {Element} The new created anchor.
 */
function anchorForId(id) {
  const anchor = document.createElement("a");
  anchor.className = "header-link";
  anchor.href = `#${id}`;
	anchor.innerHTML = '🔗';
  return anchor;
}

/**
 * Aux function to retrieve repository stars and watchers count info from
 * @param {string}	level The specific level to select header from.
 * @param {Element}	containingElement The element receiving the anchor.
 */
function linkifyAnchors(level, containingElement) {
  const headers = containingElement.getElementsByTagName(`h${level}`);
  [...headers].map(header => {
    if (typeof header.id !== "undefined" && header.id !== "") {
      header.append(anchorForId(header.id));
    }
  });
}

/**
 * Go through all headers applying linkify function
 */
function linkifyAllLevels() {
  const content = document.querySelector(".doc-content");
  [...Array(7).keys()].map(level => {
    linkifyAnchors(level, content);
  });
}

// Dropdown functions

/* When the user clicks on the navigation Documentation button,
 * toggle between hiding and showing the dropdown content.
 */
function openDropdown(e) {
  e.preventDefault();
  e.stopPropagation();
  // Calling close func. in case we're clicking another dropdown with one opened
  closeDropdown(e);
  const parent = e.target.closest("div[id$='-dropdown']");
  if (parent) {
    const dropdown = parent.querySelector(".dropdown-content");
    if (dropdown) {
      dropdown.classList.toggle("show");
      if (dropdown.classList.contains("show")) {
        document.documentElement.addEventListener("click", closeDropdown);
      }
      else {
        document.documentElement.removeEventListener("click", closeDropdown);
      }
    }
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdown(e) {
  const dropdown = document.querySelector("div[id$='-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    const currentTarget = e.currentTarget || {};
    const currentTargetParent = currentTarget.closest("div[id$='-dropdown']");
    const dropdownParent = dropdown.closest("div[id$='-dropdown']");
    if (currentTargetParent !== dropdownParent) {
      dropdown.classList.remove("show");
    }
    document.documentElement.removeEventListener("click", closeDropdown);
  }
}


window.addEventListener("DOMContentLoaded", () => {
  activateToggle();
  activateMenuNesting();
  loadGitHubStats();
  linkifyAllLevels();
});
