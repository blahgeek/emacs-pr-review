# Emacs PR Review

Review Github Pull Request from Emacs!

![](images/overview.png)


## How-to

### Install

![](https://melpa.org/packages/pr-review-badge.svg)

### Setup github token

This project uses [ghub](https://magit.vc/manual/ghub/Creating-and-Storing-a-Token.html#Creating-and-Storing-a-Token),
see its document for more details about how to setup the token.

Simply put, add the following line to `~/.authinfo` (replace `<...>` accordingly):

```
machine api.github.com login <YOUR_USERNAME>^emacs-pr-review password <YOUR_GITHUB_PERSONAL_TOKEN>
```

### Open a PR

There's two entrypoint to open a PR:
- `M-x pr-review`: open a PR with given URL.
- `M-x pr-review-search-open`: search in github and select a PR from search result.

Suggested config (especially for evil users):

```elisp
(evil-ex-define-cmd "prr" #'pr-review)
(evil-ex-define-cmd "prs" #'pr-review-search-open)
(add-to-list 'browse-url-default-handlers
             '(pr-review-url-parse . pr-review-open-url))
```

Note that `pr-review` will use the URL in current context as the default input.
It also works with the `notmuch` email client: try it when viewing the github notification email.

### Keybindings in PrReview buffer

There's three most-used keybindings:

- `C-c C-c`: add a comment based on current context.
  - When current point is on a review thread, add a comment to current thread;
  - When current point in on the changed files, add a pending review thread to current changed line; you can also add it to multiple lines by selecting a region;
  - Otherwise, add a comment to the pull request.
- `C-c C-s`: perform some "action" based on current context.
  - When current point is on a review thread, resolve current thread;
  - When current point is on the changed files, or there are any pending reviews, prompt to submit the review with action;
  - Otherwise, prompt to merge, close or re-open the PR.
- `C-c C-e`: edit the content under point based on current context, the following items can be updated (if you have the right permission):
  - PR description
  - PR title
  - Comment
  - Comment in a review thread
  - Pending review thread

There's also buttons (clickable texts) for major actions (e.g. reply, submit review), you can just use them.

Some other keybindings:

- `C-c C-r`: refresh (reload) current buffer
- `C-c C-v`: view current changed file under point (either HEAD or BASE version, based on current point) in a separated buffer
- `C-c C-o`: open this pull request in browser

Evil users will also find some familiar keybindings. See `describe-mode` for more details.

### Keybindings in PrReviewInput buffer

When you are adding or editing the comment, you will be editing in a new PrReviewInput buffer.
Keybindings in this buffer:

- `C-c C-c`: Finish editing, confirm the content
- `C-c C-k`: Abort, drop the content
- `C-c @`: Mention some other (inserting `@username`)
