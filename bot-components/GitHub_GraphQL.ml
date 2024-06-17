(*open GitHub_types*)

(* Queries *)

module PullRequest_Milestone_and_Cards =
[%graphql
{|
  fragment Column on ProjectColumn {
    id
    databaseId
  }

  query backportInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        milestone {
          title
          description
        }
        projectCards(first:100) {
          nodes {
            id
            column { ... Column }
            project {
              columns(first:100) {
                nodes { ... Column }
              }
            }
          }
        }
      }
    }
  }
|}]

module PullRequest_ID =
[%graphql
{|
  query prID($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        id
      }
    }
  }
|}]

module PullRequest_ID_and_Milestone =
[%graphql
{|
  query prInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        id
        databaseId
        milestone {
          title
          description
        }
      }
    }
  }
|}]

module TeamMembership =
[%graphql
{|
  query teamMember($org: String!, $team: String!, $user: String!) {
    organization(login:$org) {
      team(slug:$team) {
        members(query:$user, first:1) {
          nodes { login }
        }
      }
    }
  }
|}]

module ParseAsString = struct
  let parse = Yojson.Basic.to_string

  let serialize = Yojson.Basic.from_string

  type t = string
end

module PullRequest_Refs =
[%graphql
{|
  query prRefs($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name:$repo) {
      pullRequest(number: $number) {
        id
        baseRepository {
          url @ppxCustom(module: "ParseAsString")
        }
        baseRefName
        baseRefOid @ppxCustom(module: "ParseAsString")
        headRepository {
          url @ppxCustom(module: "ParseAsString")
        }
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
|}]

module Issue_Milestone =
[%graphql
{|
  fragment Milestone on Milestone {
    id
  }

  fragment PullRequest on PullRequest {
    id
    milestone { ... Milestone }
  }

  query issueMilestone($owner: String!, $repo: String!, $number: Int!) {
    repository(owner:$owner, name:$repo) {
      issue(number:$number) {
        id
        milestone { ... Milestone }
        timelineItems(itemTypes:[CLOSED_EVENT],last:1) {
          nodes {
            ... on ClosedEvent {
              closer {
                ... on PullRequest { ... PullRequest }
                ... on Commit {
                  associatedPullRequests(first: 2) {
                    nodes { ... PullRequest }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
|}]

module PullRequestReviewsInfo =
[%graphql
{|
  fragment Reviews on PullRequestReviewConnection {
    nodes {
      author { login }
      state
    }
  }

  query mergePullRequestInfo($owner: String!, $repo: String!, $number: Int!) {
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
            id
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
|}]

module DefaultBranch =
[%graphql
{|
  query defaultBranch($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      defaultBranchRef {
        name
      }
    }
  }
|}]

module FileContent =
[%graphql
{|
  query fileContent($owner: String!, $repo: String!, $file: String!) {
    repository(owner: $owner, name: $repo) {
      file:object(expression: $file) {
        ... on Blob {
          text
        }
      }
    }
  }
|}]

module RepoId =
[%graphql
{|
  query repoId($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      id
    }
  }
|}]

module GetCheckRuns =
[%graphql
{|
  query getCheckRuns($appId: Int!, $owner: String!, $repo: String!, $commit: String!, $context: String!) {
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
              id
            }
          }
        }
      }
    }
  }
|}]

module GetLabel =
[%graphql
{|
  query getLabels($owner: String!, $repo: String!, $label: String!) {
    repository(owner:$owner, name:$repo) {
      label(name: $label) {
        id
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
        id
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
      id
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

module GetPipelineSummary =
[%graphql
{|
query getChecks($appId: Int!, $owner: String!, $repo:String!, $head: String!) {
  repository(name: $repo,owner:$owner) {
    getPipelineSummaryCommit: object(expression: $head) {
      ... on Commit {
        checkSuites(first: 1,filterBy:{appId: $appId}) {
          nodes {
            getPipelineSummaryCheckRuns: checkRuns(first: 1, filterBy: {checkName:"GitLab CI pipeline (pull request)"}) {
              nodes {
                summary
              }
            }
          }
        }
      }
    }
  }
}
|}]

module GetProjectFieldValues =
[%graphql
{|
query getProjectFieldValues($organization: String!, $project: Int!, $field: String!, $options: [String!]!) {
  organization(login: $organization) {
    projectV2(number: $project) {
      id
      field(name: $field) {
        ... on ProjectV2SingleSelectField {
          id
          options(names: $options) {
            id
            name
          }
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

module LabelIssue =
[%graphql
{|
  mutation labelIssue($issue_id: ID!, $label_ids: [ID!]!) {
    addLabelsToLabelable(
      input: {labelableId: $issue_id, labelIds:$label_ids}) {
      clientMutationId
    }
  }
|}]

module UnlabelIssue =
[%graphql
{|
  mutation unlabelIssue($issue_id: ID!, $label_ids: [ID!]!) {
    removeLabelsFromLabelable(
      input: {labelableId: $issue_id, labelIds:$label_ids}) {
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
      checkRun {
        url @ppxCustom(module: "ParseAsString")
      }
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
