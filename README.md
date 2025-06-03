# 🎵 TuneFi - Music Rights Tokenization Platform

> 🚀 Democratizing music investment through blockchain technology

## 📖 Overview

TuneFi is a revolutionary smart contract platform built on Stacks that enables fans and investors to purchase shares in song royalties. Artists can tokenize their music rights, allowing supporters to invest directly in their favorite tracks and earn returns from streaming royalties.

## ✨ Key Features

- 🎤 **Artist Onboarding**: Musicians can tokenize their songs by creating shares
- 💰 **Fan Investment**: Supporters can buy shares in songs they believe in
- 📊 **Royalty Distribution**: Automated distribution of streaming royalties to shareholders
- 🔄 **Share Trading**: Transfer shares between users
- 📈 **Transparent Tracking**: Real-time visibility into investments and returns
- 🛡️ **Secure Transactions**: Built on Stacks blockchain for security and transparency

## 🏗️ Core Functions

### For Artists 🎨

#### Create a Song Token
```clarity
(contract-call? .TuneFi create-song "Song Title" "Artist Name" u1000 u100)
```
- Creates 1000 shares at 100 STX each

#### Distribute Royalties
```clarity
(contract-call? .TuneFi distribute-royalties u1 u50000)
```
- Distributes 50,000 STX in royalties for song ID 1

#### Deactivate Song
```clarity
(contract-call? .TuneFi deactivate-song u1)
```

### For Investors 💎

#### Buy Shares
```clarity
(contract-call? .TuneFi buy-shares u1 u10)
```
- Purchases 10 shares of song ID 1

#### Claim Royalties
```clarity
(contract-call? .TuneFi claim-royalties u1 u1)
```
- Claims royalties from distribution ID 1 for song ID 1

#### Transfer Shares
```clarity
(contract-call? .TuneFi transfer-shares u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 u5)
```
- Transfers 5 shares to another user

## 📊 Read-Only Functions

### Get Song Information
```clarity
(contract-call? .TuneFi get-song-info u1)
```

### Check User Shares
```clarity
(contract-call? .TuneFi get-user-shares u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Calculate Potential Royalties
```clarity
(contract-call? .TuneFi calculate-user-royalties u1 u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
```

```bash
cd tunefi
```

```bash
clarinet check
```

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy --testnet
```

## 💡 Use Cases

### 🎵 For Musicians
- **Early Funding**: Get upfront capital by selling future royalty shares
- **Fan Engagement**: Create deeper connections with supporters
- **Transparent Revenue**: Show fans exactly how their investment performs

### 🎧 For Fans & Investors
- **Support Artists**: Directly invest in musicians you believe in
- **Earn Returns**: Receive passive income from streaming royalties
- **Portfolio Diversification**: Build a music investment portfolio# TuneFi

