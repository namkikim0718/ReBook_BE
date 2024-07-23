package com.be.rebook.domain.members.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Getter
@Setter
public class RefreshTokens {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "refresh_id")
    private Long refreshId;

    private String username; //하나의 유저가 여러개의 토큰을 발급받을 수 있음
    private String refresh;
    private String expiration;
}
