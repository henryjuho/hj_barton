from scholarly import scholarly


def main():

    search_query = scholarly.search_author('Henry J Barton')
    author = next(search_query).fill(sections=['publications'])

    print('| paper | cited | year |\n|:---------|:------:|:---------:|')

    out_lines = []

    for pub in author.publications:

        try:
            pub = pub.fill()
            year = int(pub.bib['year'])
            cites = pub.bib['cites']

            authors = pub.bib['author'].replace('Henry J Barton', '**Henry J Barton**').replace(' and', ',')

            paper_str = '**[{title}]({link})**<br>{authors}<br>{journal} {vol}({issue}), {pages}'.format(
                title=pub.bib['title'], link=pub.bib['url'], authors=authors, journal=pub.bib['journal'],
                vol=pub.bib['volume'], issue=pub.bib['number'], pages=pub.bib['pages']
            )

        except KeyError:
            continue

        line = '| {} | {} | {} |'.format(paper_str, cites, year)
        out_lines.append([year, line])

    for paper in sorted(out_lines, key=lambda x: x[0], reverse=True):
        print(paper[1])


if __name__ == '__main__':
    main()
