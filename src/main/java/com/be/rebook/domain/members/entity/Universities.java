package com.be.rebook.domain.members.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Getter
public class Universities {
    @Id
    @Column(name = "unv_id")
    private Long unvId;

    private String university;
}
