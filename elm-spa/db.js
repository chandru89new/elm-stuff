(function () {
  'use-strict';
  const { getItem, setItem } = window.localStorage || {}

  if (!getItem || !setItem) throw new Error('No localstorage support found')

  window.saveData = key => data => {
    return localStorage.setItem(key, JSON.stringify(data))
  }

  window.getData = (key) => (defaultValue) => {
    if (localStorage.getItem(key)) {
      return JSON.parse(localStorage.getItem(key))
    }
    return defaultValue
  }

})()