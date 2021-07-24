import algoliasearch from 'algoliasearch';

const client = algoliasearch('9ALKW0EDLK', 'e26f227f28a5434dd8f751fce0959525');
const index = client.initIndex('elisp-demos');

const $searchBox = document.querySelector('#searchBox input[type=search]');
const $hits = document.querySelector('#hits');

function renderHits(query) {
  index.search(query).then((result) => {
    // Please sanitize user-provided data when using `innerHTML` to avoid XSS
    $hits.innerHTML = `
      <ol class="ais-hits">
        ${result.hits
          .map(
            (hit) =>
              `<li class="ais-hits--item">
      <article>
      <h1>${hit._highlightResult.name.value}</h1>
      <h5>Example</h5>
      <p><code>${hit.demo_src}</code></p>
      <h5>Result</h5>
      <p><code>${hit.demo_result}</code></p>
      </article>
      </li>`
          )
          .join('')}
      </ol>`;
  });
}

$searchBox.addEventListener('input', (event) => {
  const query = event.target.value;

  renderHits(query);
});

renderHits('');
