var app = Elm.Main.init({
  flags: {
    todos: getData('todos')([]),
    projects: getData('projects')([])
  }
});
app.ports.saveTodos.subscribe((data) => {
  saveData('todos')(data)
})
app.ports.saveProjects.subscribe((data) => {
  saveData('projects')(data)
})
app.ports.showDeleteConfirmation.subscribe((id) => {
  const shouldDelete = window.confirm(`Sure? (${id})`)
  app.ports.processDeleteConfirmation.send([shouldDelete, id])
})

const onClickAway = (e) => {
  const id = e.target.id
  const whitelist = [
    'add-project-input',
    'add-project-link',
    'add-todo-link',
    'add-todo-input'
  ]
  if (whitelist.includes(id)) {
    return
  }
  app.ports.hideInputs.send(true)
}

const onEscapeKeyPressed = keyEvent => {
  if (keyEvent.keyCode === 27 || keyEvent.key === "Escape") {
    app.ports.hideInputs.send(true)
  }
}

document.addEventListener('click', onClickAway)

document.addEventListener('keyup', onEscapeKeyPressed)