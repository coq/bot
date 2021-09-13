module ParseAsString = struct
  type t = string

  let parse = Yojson.Basic.to_string

  let serialize = Yojson.Basic.from_string
end

module ID : sig
  type t

  val parse : string -> t

  val serialize : t -> string

  val equal : t -> t -> bool
end = struct
  type t = string

  let parse x = x

  let serialize x = x

  let equal = String.equal
end

module Queries =
[%graphql
{|
  fragment Column on ProjectColumn {
    id @ppxCustom(module: "ID")
    databaseId
  }

  fragment Project on Project {
    columns(first:100) {
      nodes { ... Column }
    }
  }

  fragment ProjectCards on ProjectCardConnection {
    nodes {
      id @ppxCustom(module: "ID")
      column { ... Column }
      project { ... Project }
    }
  }

  fragment Milestone on Milestone {
    id @ppxCustom(module: "ID")
    number
    title
    description
  }

  fragment PullRequestWithMilestone on PullRequest {
    id @ppxCustom(module: "ID")
    databaseId
    milestone { ... Milestone }
  }

  fragment PullRequestWithMilestoneAndCards on PullRequest {
    id @ppxCustom(module: "ID")
    databaseId
    milestone { ... Milestone }
    projectCards(first:100) { ... ProjectCards }
  }

  fragment Reviews on PullRequestReviewConnection {
    nodes {
      author { login }
      state
    }
  }

  fragment RepositoryInfo on Repository {
    id @ppxCustom(module: "ID")
    defaultBranchRef { name }
  }

  query GetPullRequestMilestone($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) { ... PullRequestWithMilestone }
    }
  }

  query GetPullRequestMilestoneAndCards($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) { ... PullRequestWithMilestoneAndCards }
    }
  }

  query CheckTeamMembership($org: String!, $team: String!, $user: String!) {
    organization(login:$org) {
      team(slug:$team) {
        members(query:$user, first:1) {
          nodes { login }
        }
      }
    }
  }

  query GetPullRequestRefs($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name:$repo) {
      pullRequest(number: $number) {
        id @ppxCustom(module: "ID")
        baseRefName
        baseRefOid @ppxCustom(module: "ParseAsString")
        headRefName
        headRefOid @ppxCustom(module: "ParseAsString")
        merged
        commits(last: 1) {
          nodes {
            commit {
              message
            }
          }
        }
      }
    }
  }

  query GetIssueMilestoneAndCloseEvent($owner: String!, $repo: String!, $number: Int!) {
    repository(owner:$owner, name:$repo) {
      issue(number:$number) {
        id @ppxCustom(module: "ID")
        milestone { ... Milestone }
        timelineItems(itemTypes:[CLOSED_EVENT],last:1) {
          nodes {
            ... on ClosedEvent {
              closer {
                ... on PullRequest { ... PullRequestWithMilestone }
                ... on Commit {
                  associatedPullRequests(first: 2) {
                    nodes { ... PullRequestWithMilestone }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  query GetPreMergeInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        baseRef {
          name
        }
        files(first: 100) {
          nodes {
            path
          }
        }
        comments(last:10) {
          nodes {
            id @ppxCustom(module: "ID")
            author {
              login
            }
            createdViaEmail
          }
        }
        reviewDecision
        latestReviews(first: 100) { ... Reviews }
        latestOpinionatedReviews(first: 100) { ... Reviews }
      }
    }
  }

  query GetRepositoryInfo($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) { ... RepositoryInfo }
  }

  query GetFileContent($owner: String!, $repo: String!, $file: String!) {
    repository(owner: $owner, name: $repo) {
      file:object(expression: $file) {
        ... on Blob {
          text
        }
      }
    }
  }

  query GetCheckRuns($appId: Int!, $owner: String!, $repo: String!, $commit: String!, $context: String!) {
    repository(owner:$owner, name:$repo) {
      obj: object(expression: $commit) {
        ... on Commit {
          checkSuites(first: 1, filterBy: { appId: $appId }) {
            nodes {
              checkRuns(first: 1, filterBy: { checkName: $context }) {
                nodes {
                  databaseId
                }
              }
            }
          }
          status {
            context(name: $context) {
              id @ppxCustom(module: "ID")
            }
          }
        }
      }
    }
  }

  query GetLabels($owner: String!, $repo: String!, $label: String!) {
    repository(owner:$owner, name:$repo) {
      label(name: $label) {
        id @ppxCustom(module: "ID")
      }
    }
  }
|}]

module GetOpenPullRequestWithLabel =
[%graphql
{|

query getOpenPullRequestWithLabel($owner: String!, $repo:String!, $label:String!, $cursor: String, $len: Int!) {
  repository(name: $repo,owner:$owner) {
    pullRequests(first: $len, labels: [$label], states: [OPEN], after: $cursor) {
      nodes {
        id @ppxCustom(module: "ID")
        number
      }
      pageInfo {
        endCursor
        hasNextPage
      }
    }
  }
}

|}]

module GetPullRequestLabelTimeline =
[%graphql
{|
fragment Label on Label {
  name
}

query getPullRequestLabelTimeline($owner: String!, $repo:String!, $prNumber: Int!, $cursor: String, $len: Int!) {
  repository(name: $repo,owner:$owner) {
    pullRequest(number: $prNumber) {
      timelineItems(itemTypes: [LABELED_EVENT, UNLABELED_EVENT], after: $cursor, first: $len) {
        nodes {
          ... on LabeledEvent {
            labeledAt: createdAt
            labelAdded: label { ... Label }
          }
          ... on UnlabeledEvent {
            unlabeledAt: createdAt
            labelRemoved: label { ... Label }
          }
        }
        pageInfo {
          endCursor
          hasNextPage
        }
      }
    }
  }
}

|}]

module GetPullRequestLabels =
[%graphql
{|

query getPullRequestLabels($owner: String!, $repo:String!, $prNumber: Int!, $cursor: String, $len: Int!) {
  repository(name: $repo,owner:$owner) {
    pullRequest(number: $prNumber) {
      labels (after: $cursor, first: $len) {
        nodes {
          name
        }
        pageInfo {
          endCursor
          hasNextPage
        }
      }
    }
  }
}

|}]

module GetBaseAndHeadChecks =
[%graphql
{|
fragment CheckRuns on CheckRunConnection {
  nodes {
    name
    conclusion
    summary
    text
  }
}

fragment CheckSuites on CheckSuiteConnection {
  nodes {
    checkRuns(first: 100) { ... CheckRuns }
  }
}

query getChecks($appId: Int!, $owner: String!, $repo:String!, $prNumber: Int!, $base: String!, $head: String!) {
  repository(name: $repo,owner:$owner) {
    pullRequest(number: $prNumber) {
      id @ppxCustom(module: "ID")
      labels(first: 100) {
        nodes {
          name
        }
      }
      isDraft
    }
    base: object(expression: $base) {
      ... on Commit {
        checkSuites(first: 1,filterBy:{appId: $appId}) {
           ... CheckSuites
        }
      }
    }
    head: object(expression: $head) {
      ... on Commit {
        checkSuites(first: 1,filterBy:{appId: $appId}) {
           ... CheckSuites
        }
      }
    }
  }
}
|}]

(* Mutations *)

module MoveCardToColumn =
[%graphql
{|
  mutation moveCard($card_id:ID!,$column_id:ID!) {
    moveProjectCard(input:{cardId:$card_id,columnId:$column_id}) {
      clientMutationId
    }
  }
|}]

module PostComment =
[%graphql
{|
  mutation addComment($id:ID!,$message:String!) {
    payload: addComment(input:{subjectId:$id,body:$message}) {
      commentEdge {
        node {
          url @ppxCustom(module: "ParseAsString")
        }
      }
    }
  }
|}]

module UpdateMilestone =
[%graphql
{|
  mutation updateMilestone($issue: ID!, $milestone: ID!) {
    updateIssue(input: {id: $issue, milestoneId: $milestone}) {
      clientMutationId
    }
  }
|}]

module MergePullRequest =
[%graphql
{|
  mutation mergePullRequest($pr_id: ID!, $commit_headline: String,
  $commit_body: String, $merge_method: PullRequestMergeMethod) {
    mergePullRequest(
      input: {pullRequestId: $pr_id, commitHeadline: $commit_headline,
      commitBody: $commit_body, mergeMethod: $merge_method}) {
      pullRequest {
        merged
        mergedAt
        state
        url
      }
    }
  }
|}]

module ClosePullRequest =
[%graphql
{|
  mutation closePullRequest($pr_id: ID!) {
    closePullRequest(
      input: {pullRequestId: $pr_id}) {
      pullRequest {
        state
      }
    }
  }
|}]

module LabelPullRequest =
[%graphql
{|
  mutation labelPullRequest($pr_id: ID!, $label_ids: [ID!]!) {
    addLabelsToLabelable(
      input: {labelableId: $pr_id, labelIds:$label_ids}) {
      clientMutationId
    }
  }
|}]

module UnlabelPullRequest =
[%graphql
{|
  mutation unlabelPullRequest($pr_id: ID!, $label_ids: [ID!]!) {
    removeLabelsFromLabelable(
      input: {labelableId: $pr_id, labelIds:$label_ids}) {
      clientMutationId
    }
  }
|}]

module NewCheckRun =
[%graphql
{|
  mutation newCheckRun($name: String!, $repoId: ID!, $headSha: String!,
  $status: RequestableCheckStatusState!, $title: String!, $text: String, $summary: String!,
  $url: String!, $conclusion: CheckConclusionState, $externalId: String) {
    createCheckRun(
      input: {
        status:$status,
        name:$name,
        repositoryId:$repoId,
        headSha:$headSha,
        conclusion:$conclusion,
        detailsUrl:$url,
        output:{
          title:$title,
          text:$text,
          summary:$summary
        }
        externalId:$externalId
      }) {
      clientMutationId
    }
  }
|}]

module UpdateCheckRun =
[%graphql
{|
  mutation updateCheckRun($checkRunId: ID!, $repoId: ID!
  $conclusion: CheckConclusionState!, $title: String!, $text: String,
  $url: String, $summary: String!) {
    updateCheckRun(
      input: {
        checkRunId:$checkRunId,
        repositoryId:$repoId,
        conclusion:$conclusion,
        detailsUrl:$url,
        output:{
          title:$title,
          text:$text,
          summary:$summary
        }
      }) {
      clientMutationId
    }
  }
|}]
